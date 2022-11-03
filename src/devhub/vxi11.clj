(ns devhub.vxi11
  ^{:author "Thomas Bock <wactbprot@gmail.com>"
    :doc "Handles VXI11 Actions."}
  (:require [devhub.utils :as u]
            [devhub.safe :as safe]
            [clojure.string :as string]
            [com.brunobonacci.mulog :as µ])
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

(def usr (VXI11UserFactory/create)) 
(def ctrls (atom {}))
(def devs (atom {}))

(defn get-ctrl [host dev-name]
  (let [sig (keyword (str host dev-name))
        ctrl (sig @ctrls)]
    (if ctrl ctrl
        (do
          (let [ctrl (VXI11Factory/create host dev-name)]
            ;; _    (.setIoTimeout ctrl (:vxi-io-timeout conf))
            ;; _    (.setLockTimeout ctrl (:vxi-lock-timeout conf))                     
            (sig (swap! ctrls assoc sig ctrl)))))))

(comment
  (defn get-dev [ctrl host dev-name p-adr s-adr]
  (let [sig (keyword (str host dev-name p-adr s-adr))
        dev (sig @devs)]
    (if dev  dev
      (do
        (let [dev (.createDevice ctrl p-adr s-adr)]
         (sig (swap! devs assoc sig dev))))))))

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
  [{conf :vxi} {host :Host dev-name :DeviceName p-adr :PrimaryAddress s-adr :SecondaryAddress :as task}]
  ;;(if-not (u/connectable? task)
  ;; {:error "can not connect"}
  (let [ctrl (get-ctrl host dev-name)
        _    (.connect ctrl usr)
        dev  (get-dev ctrl host dev-name p-adr s-adr)
        _    (.connect dev usr)
        bs   (:read-buffer-size conf)
        bb   (byte-array bs) 
        f    (fn [cmd]
               (when (seq cmd) (.write dev usr (.getBytes cmd) (.length cmd)))
               (when-not (:NoReply task) (.read dev usr bb bs))
               (bb->string bb))
        data (u/run f conf task)]
    (.setREN ctrl usr false)
    ;;    _    (.remote dev usr)    (.local dev usr)
    (.disconnect dev)
    data))

(defn handler
  "Handles VXI11 queries. "
  [conf {error :error :as task}]
  (µ/trace ::handler [:function "vxi11/handler"]
            (if error task
              (let [{error :error :as data} (query conf task)]
                (merge task (if error data (u/reshape data)))))))
