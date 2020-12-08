(ns devhub.vxi11
  (:require [devhub.utils          :as u]
            [devhub.conf           :as c]
            [clojure.string        :as string]
            [clojure.tools.logging :as log])
  (:import  [jvxi11 VXI11Factory VXI11UserFactory]))

(defn parse-gpib-str
  [s]
  (let [v (re-find (re-matcher #"(gpib[0-9]*),([0-9]*)" s))]
    {:device (nth v 1)
     :pri-adr (u/number (nth v 2))
     :sec-adr 0}))

(defn parse-inst-str [s] {:device s :pri-adr -1 :sec-adr -1})
      
(defn parse-device-str
  [s]
  (cond
    (string/starts-with? s "inst") (parse-inst-str s)
    (string/starts-with? s "gpib") (parse-gpib-str s)
    :else (throw "imprement me")))

(defn bb->val [b] (string/replace (slurp b) #" " ""))

(defn send-receive
  "Sends a `cmd` to a vxi11 device. Data is read out by means of the
  byte buffer `bb` with the size `bs`."
  [bb bs cmd dev usr]
  (.write dev usr (.getBytes cmd) (.length cmd))
  (let [t0 (u/ms)
        _  (.read dev usr bb bs)
        t1 (u/ms)]
    (u/add-times {:_x (bb->val bb)} t0 t1)))

(defn query
  "Sends the `cmds` to a vxi11 device. `repeat`s and `wait`s in between."
  [conf {h :host d :device pa :pri-adr sa :sec-adr} cmds wait repeat]
  (let [ctrl (VXI11Factory/create h d)
        usr  (VXI11UserFactory/create)]
    (.connect ctrl usr)
    (let [dev (.createDevice ctrl pa sa)]
      (.connect dev usr)
      (let [bs   (:read-buffer-size conf)
            bb   (byte-array bs) 
            data (mapv (fn [_]
                         (let [v (mapv (fn [cmd] (send-receive bb bs cmd dev usr)) cmds)]
                           (Thread/sleep wait)
                           v))
                       (range repeat))]
        (.disconnect dev)
        data))))

(defn handler
  "Handles VXI11 queries.
  
  Example:
  ```clojure
  (handler (:vxi (c/config))
           {:Wait 10 :Repeat 3 :Device \"inst0\" :Host \"e75440\" :Value \"return_50()\n\"})

  (handler (:vxi (c/config))
           {:Wait 10 :Repeat 3 :Device \"gpib0,4\" :Host \"e75416\" :Value \"MEAS:PRES?\n\"})  

  ```"
  [vxi-conf {w :Wait r :Repeat d :Device h :Host v :Value }]
  (if (and v h d )
    (let [dev-adr (parse-device-str d)
          conn    (assoc dev-adr :host h)
          wait    (if w (u/number w) (:min-wait vxi-conf))
          repeat  (if r (u/number r) (:repeat vxi-conf))
          cmds (if (string? v) [v] v)]
      (if-let [data (query vxi-conf conn cmds wait repeat)]
        {:data (u/meas-vec data)}
        {:error true :reason "no data"}))
    {:error true :reason "missing <value>, <host> or <device>"}))
