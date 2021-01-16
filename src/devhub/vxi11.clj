(ns devhub.vxi11
  ^{:author "Wact B. Prot <wactbprot@gmail.com>"
    :doc "Handles VXI11 Actions."}
  (:require [devhub.utils           :as u]
            [devhub.safe            :as safe]
            [clojure.string         :as string]
            [com.brunobonacci.mulog :as µ])
  (:import  [jvxi11 VXI11Factory VXI11UserFactory]))

(defn bb->string
  "Turns the byte buffer `b` into a string.

  Example:
  ```clojure
  (time (bb->string (byte-array 1024)))
  ;; =>
  ;; Elapsed time: 0.347679 msecs
  ;; \"\"
  ```"
  [b]
  (string/replace (slurp b) #" " ""))

(defn query
  "Handles VXI11 queries. Sends the `cmds` to a vxi11 device. Data is
  read out by means of the byte buffer `bb` with the size `bs`.
  
  Example:
  ```clojure
  (def c (u/config))
  (def t {:Host \"e75465\" :Value [\":meas:func\"] :Wait 10 :Repeat 2
          :DeviceName \"gpib0\" :PrimaryAddress 8 :SecondaryAddress 0})
  (query c t)
  ```"
  [{conf :vxi} task]
  (let [{host :Host device :DeviceName pa :PrimaryAddress sa :SecondaryAddress} task]
    (µ/log ::query :req-id (:req-id task) :Host host :DeviceName device)
    (let [ctrl (VXI11Factory/create host device)
          usr  (VXI11UserFactory/create)
          _    (.connect ctrl usr)
          dev  (.createDevice ctrl pa sa)
          conn (try (.connect dev usr) 
                    (catch Exception e
                      (let [msg "can not connect to device"]
                        (µ/log ::query :error msg :req-id (:req-id task))
                        {:error msg})))]
      (if (:error conn)
        (merge task conn)
        (let [bs (:read-buffer-size conf)
              bb (byte-array bs) 
              f  (fn [cmd]
                   (.write dev usr (.getBytes cmd) (.length cmd))
                   (when-not (:NoReply task) (.read dev usr bb bs))
                   (bb->string bb))
              data (u/run f conf task)]
          (.disconnect dev)
          (merge task (u/reshape data)))))))
