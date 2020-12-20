(ns devhub.vxi11
  (:require [devhub.utils   :as u]
            [devhub.safe   :as safe]
            [clojure.string :as string])
  (:import  [jvxi11 VXI11Factory VXI11UserFactory]))

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
         rep    :Repeat
         norep  :NoReply} task
        ctrl (VXI11Factory/create host device)
        usr  (VXI11UserFactory/create)]
    (.connect ctrl usr)
    (let [dev (.createDevice ctrl pa sa)]
      (.connect dev usr)
      (let [bs   (:read-buffer-size conf)
            bb   (byte-array bs) 
            f    (fn [cmd] (send-receive bb bs dev usr cmd))
            data (u/run f cmds wait rep)]
        (.disconnect dev)
        data))))

(defn handler
  "Handles VXI11 queries.
  
  Example:
  ```clojure
  (def vc (u/config))
  (handler vc {:Device \"gpib0,8\" :Host \"e75465\" :Value \":meas:func\"})

  (handler vc {:Wait 10 :Repeat 3 :Device \"inst0\" :Host \"e75495\" :Value \"ch101()\n\"})

  ;; =>
  ;; {:data
  ;; {:_x [MEASURING  1.015],
  ;; :_t_start [1607513740001],
  ;; :_t_stop [1607513740028],
  ;; :_dt [27]}}
  ```"
  [{conf :vxi} task]
  (if-let [task (safe/vxi conf task)]
    (if-let [data (query conf task)]
      (u/meas-vec data)
      {:error true :reason "no data"})
    {:error true :reason "missing <value>, <host> or <device>"}))
