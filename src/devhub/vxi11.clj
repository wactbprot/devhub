(ns devhub.vxi11
  ^{:author "Wact B. Prot <wactbprot@gmail.com>"
    :doc "Handles VXI11 Actions."}
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
  (let [{pa :PrimaryAddress sa :SecondaryAddress host :Host device :DeviceName} task
        ctrl (VXI11Factory/create host device)
        usr  (VXI11UserFactory/create)
        _    (.connect ctrl usr)]
    (let [dev-or-err (try (.createDevice ctrl pa sa)
                          (catch Exception e
                            {:error "can not connect to device"}))]
      (if (:error dev-or-err)
        dev-or-err
        (let [dev  dev-or-err
              _    (.connect dev usr)
              bs   (:read-buffer-size conf)
              bb   (byte-array bs) 
              f    (fn [cmd]
                     (.write dev usr (.getBytes cmd) (.length cmd))
                     (when-not (:NoReply task) (.read dev usr bb bs))
                     (u/bb->string bb))
              data (u/run f conf task)]
          (.disconnect dev)
          data)))))

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
    (let [data-or-err (query conf task)]
      (if (:error data-or-err)
        (let [error   data-or-err
              err-msg (:error error)]
          (µ/log ::handler :error err-msg :req-id (:req-id task))
          error)
        (let [data (u/meas-vec data-or-err)]
          (µ/log ::handler :data data  :req-id (:req-id task))
          data))))
