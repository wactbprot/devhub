(ns devhub.vxi11
  (:require [devhub.utils   :as u]
            [devhub.conf    :as c]
            [clojure.string :as string])
  (:import  [jvxi11 VXI11Factory VXI11UserFactory]))

(defn parse-gpib-str
  [s]
  (when-let [v (re-find (re-matcher #"(gpib[0-9]*),([0-9]*),([0-9]*)" s))]
    {:DeviceName       (nth v 1)
     :PrimaryAddress   (u/number (nth v 2))
     :SecondaryAddress (u/number (nth v 3 0))}))

(defn parse-inst-str [s] {:DeviceName s :PrimaryAddress -1 :SecondaryAddress -1})
      
(defn parse-device-str
  [s]
  (when (string? s)
    (cond
      (string/starts-with? s "inst") (parse-inst-str s)
      (string/starts-with? s "gpib") (parse-gpib-str s)
      :else (throw "imprement me"))))

(defn bb->string [b] (string/replace (slurp b) #" " ""))

(defn send-receive
  "Sends a `cmd` to a vxi11 device. Data is read out by means of the
  byte buffer `bb` with the size `bs`."
  [bb bs dev usr cmd]
  (.write dev usr (.getBytes cmd) (.length cmd))
  (let [t0 (u/ms)  _ (.read dev usr bb bs) t1 (u/ms)]
    (u/add-times {:_x (bb->string bb)} t0 t1)))

(defn query
  "Sends the `cmds` to a vxi11 device. `repeat`s and `wait`s in between.

  TODO: Calculate and set TimeOut.
  "
  [conf task]
  (let [{host   :Host
         device :DeviceName
         pa     :PrimaryAddress
         sa     :SecondaryAddress
         cmds   :Value
         wait   :Wait
         repeat :Repeat
         norep  :NoReply} task
        ctrl (VXI11Factory/create host device)
        usr  (VXI11UserFactory/create)]
    (.connect ctrl usr)
    (let [dev (.createDevice ctrl pa sa)]
      (.connect dev usr)
      (let [bs   (:read-buffer-size conf)
            bb   (byte-array bs) 
            pf   (partial (fn [cmd] (send-receive bb bs dev usr cmd)))
            data (u/run pf cmds wait repeat)]
        (.disconnect dev)
        data))))

(defn safe
  [conf task]
  (let [{h :Host d :Device v :Value w :Wait r :Repeat n :NoReply} task
        m (parse-device-str d)]
    (when (and v h m) (assoc (merge task m)
                             :Value   (if (string? v) [v] v)
                             :Wait    (if w (u/number w) (:min-wait conf))
                             :Repeat  (if r (u/number r) (:repeat conf))
                             :NoReply (if n n false)))))
(defn handler
  "Handles VXI11 queries.
  
  Example:
  ```clojure
  (def vc (:vxi (c/config)))
  (handler vc {:Device \"gpib0,8\" :Host \"e75465\" :Value \":meas:func\"})
  
  ;; =>
  ;; {:data
  ;; {:_x [MEASURING  1.015],
  ;; :_t_start [1607513740001],
  ;; :_t_stop [1607513740028],
  ;; :_dt [27]}}
  ```"
  [conf task]
  (if-let [task (safe conf task)]
    (if-let [data (query conf task)]
      {:data (u/meas-vec data)}
      {:error true :reason "no data"})
    {:error true :reason "missing <value>, <host> or <device>"}))
