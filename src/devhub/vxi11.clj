(ns devhub.vxi11
  (:require [devhub.utils           :as u]
            [devhub.safe            :as safe]
            [com.brunobonacci.mulog :as µ])
  (:import  [jvxi11 VXI11Factory VXI11UserFactory]))

(defn query
  "Sends the `cmds` to a vxi11 device. Data is read out by means of the
  byte buffer `bb` with the size `bs`.

  TODO: Calculate and set TimeOut.
  "
  [conf task]
  (let [{host :Host       device :DeviceName
         pa   :PrimaryAddress sa :SecondaryAddress
         norep :NoReply} task
        ctrl (VXI11Factory/create host device)
        usr  (VXI11UserFactory/create)]
    (.connect ctrl usr)
    (let [dev (.createDevice ctrl pa sa)]
      (.connect dev usr)
      (let [bs   (:read-buffer-size conf)
            bb   (byte-array bs) 
            f    (fn [cmd]
                   (.write dev usr (.getBytes cmd) (.length cmd))
                   (when-not norep
                     (.read dev usr bb bs))
                   (u/bb->string bb))
            data (u/run f conf task)]
        (.disconnect dev)
        data))))

(defn handler
  "Handles VXI11 queries.
  
  Example:
  ```clojure
  (def c (u/config))
  (handler c {:Device \"gpib0,8\" :Host \"e75465\" :Value \":meas:func\"})
  ;; =>
  ;; {:data
  ;; {:_x [MEASURING  1.015],
  ;; :_t_start [1607513740001],
  ;; :_t_stop [1607513740028],
  ;; :_dt [27]}}
  ```"
  [{conf :vxi} task]
  (prn task)
  (if-let [task (safe/vxi conf task)]
    (if-let [data (u/exec-with-try (fn [] (query conf task)))]
      (u/meas-vec data)
      {:error true :reason "no data"})
    {:error true :reason "missing <value>, <host> or <device>"}))
