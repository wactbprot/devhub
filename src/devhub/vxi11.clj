(ns devhub.vxi11
  ^{:author "Wact B. Prot <wactbprot@gmail.com>"
    :doc "Handles VXI11 Actions."}
  (:require [devhub.utils           :as u]
            [devhub.safe            :as safe]
            [clojure.string         :as string]
            [com.brunobonacci.mulog :as mu])
  (:import  [jvxi11 VXI11Factory VXI11UserFactory]
            [java.net InetAddress]))

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
  "Sends the `cmds` to a vxi11 device. Data is
  read out by means of the byte buffer `bb` with the size `bs`.
  
  Example:
  ```clojure
  (def c (u/config))
  (def t {:Host \"e75465\" :Value [\":meas:func\"] :Wait 10 :Repeat 2
          :DeviceName \"gpib0\" :PrimaryAddress 8 :SecondaryAddress 0})
  (query c t)
  ```"
  [{conf :vxi} task]
  (if-not (u/connectable? task)
    {:error "can not connect"}
    (let [ctrl (VXI11Factory/create (:Host task) (:DeviceName task))
          usr  (VXI11UserFactory/create)
          _    (.connect ctrl usr)
          _    (.setIoTimeout ctrl (:vxi-io-timeout conf))
          _    (.setLockTimeout ctrl (:vxi-lock-timeout conf))
          dev  (.createDevice ctrl (:PrimaryAddress task) (:SecondaryAddress task))
          _    (.connect dev usr) 
          bs   (:read-buffer-size conf)
          bb   (byte-array bs) 
          f    (fn [cmd]
                 (.write dev usr (.getBytes cmd) (.length cmd))
                 (when-not (:NoReply task) (.read dev usr bb bs))
                 (bb->string bb))
          data (u/run f conf task)]
      (.disconnect dev)
      data)))

(defn handler
  "Handles VXI11 queries. "
  [conf {host :Host device :Device req-id :req-id error :error :as task}]
  (mu/trace ::handler [:function "vxi11/handler"]
            (if error
              task
              (let [_    (mu/log ::query :req-id req-id :Host host :Device device)
                    data (query conf task)]
                (merge task (if (:error data)
                              (let [msg (:error data)]
                                (mu/log ::query :error msg :req-id req-id)
                                data)
                              (let [msg "received data"]
                                (mu/log ::query :message msg :req-id req-id)
                                (u/reshape data))))))))
  
