(ns devhub.vxi11
  ^{:author "Wact B. Prot <wactbprot@gmail.com>"
    :doc "Handles VXI11 Actions."}
  (:require [devhub.utils           :as u]
            [devhub.safe            :as safe]
            [com.brunobonacci.mulog :as µ])
  (:import  [jvxi11 VXI11Factory VXI11UserFactory]))

(defn query
  "Handles VXI11 queries. Sends the `cmds` to a vxi11 device. Data is
  read out by means of the byte buffer `bb` with the size `bs`.
  
  Example:
  ```clojure
  (def c (u/config))
  (def t {:Host \"e75465\" :Value [\":meas:func\"] :Wait 10 :Repeat 2
          :DeviceName \"gpib0\" :PrimaryAddress 8 :SecondaryAddress 0})
  (query c t)
  ;; =>
  ;; [[{:_x MEASURING  2.006,
  ;;  :_t_start 1609749524988,
  ;;  :_t_stop 1609749525020,
  ;;  :_dt 32}]
  ;; [{:_x MEASURING  2.006,
  ;;  :_t_start 1609749525031,
  ;;  :_t_stop 1609749525064,
  ;;  :_dt 33}]]
  ```"
  [{conf :vxi} task]
  (let [{host :Host device :DeviceName pa :PrimaryAddress sa :SecondaryAddress} task]
    (µ/log ::query :req-id (:req-id task) :Host host :DeviceName device)
    (let [ctrl (VXI11Factory/create host device)
          usr  (VXI11UserFactory/create)
          _    (.connect ctrl usr)
          dev  (.createDevice ctrl pa sa)
          conn-or-err (try (.connect dev usr) 
                           (catch Exception e
                             (µ/log ::query :error "connection error" :req-id (:req-id task))
                             {:error "can not connect"}))]
      (if (:error conn-or-err) conn-or-err
          (let [bs (:read-buffer-size conf)
                bb (byte-array bs) 
                f  (fn [cmd]
                     (.write dev usr (.getBytes cmd) (.length cmd))
                     (when-not (:NoReply task) (.read dev usr bb bs))
                     (u/bb->string bb))
                data (u/run f conf task)]
            (.disconnect dev)
            data)))))
