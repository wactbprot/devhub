(ns devhub.vxi11
  ^{:author "Thomas Bock <wactbprot@gmail.com>"
    :doc "Handles VXI11 Actions."}
  (:require [devhub.utils :as u]
            [devhub.safe :as safe]
            [clojure.string :as string]
            [clojure.reflect :as r]
            [clojure.inspector :as i]
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

(defonce usrs (atom {}))
(defonce ctrls (atom {}))
(defonce devs (atom {}))

(defn param->sign
  ([h d] (keyword (str h d)))
  ([h d p s] (keyword (str h d p s))))

(defn usr-ok? [usr] (and usr (nil? (.getString usr))))

(defn dev-alive? [dev] (and dev (.isConnected dev)))

(defn ctrl-alive? [ctrl] (and ctrl (.isConnected ctrl)))

(defn get-usr [host dev-name]
  (let [sign (param->sign host dev-name)
        usr (sign @usrs)]
    (if-not (usr-ok? usr) 
      (sign (swap! usrs assoc sign (VXI11UserFactory/create)))
      usr)))

(defn get-ctrl [host dev-name]
  (let [sign (param->sign host dev-name)
        ctrl (sign @ctrls)
        usr (get-usr host dev-name)]
    (if-not (ctrl-alive? ctrl)
      (let [ctrl (VXI11Factory/create host dev-name)]
                                        ;(.setIoTimeout ctrl (:vxi-io-timeout conf))
                                        ;(.setLockTimeout ctrl (:vxi-lock-timeout conf))                     
        (.connect ctrl usr)
        (sign (swap! ctrls assoc sign ctrl)))
      ctrl)))

(defn get-dev [host dev-name p-adr s-adr]
  (let [sign (param->sign host dev-name p-adr s-adr)
        ctrl (get-ctrl host dev-name)
        dev (sign @devs)
        usr (get-usr host dev-name)]
    (if-not (dev-alive? dev) 
      (let [dev (.createDevice ctrl p-adr s-adr)]
        (.connect dev usr)
        (sign (swap! devs assoc sign dev)))
      dev)))

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
  (let [usr  (get-usr host dev-name)
        ctrl (get-ctrl host dev-name)
        dev  (get-dev host dev-name p-adr s-adr)
        bs   (:read-buffer-size conf)
        bb   (byte-array bs) 
        f    (fn [cmd]
               (when (seq cmd) (.write dev usr (.getBytes cmd) (.length cmd)))
               (when-not (:NoReply task) (.read dev usr bb bs))
               (bb->string bb))
        data (u/run f conf task)
        _ (.setREN ctrl usr false)
        ]
    data))

(defn handler
  "Handles VXI11 queries."
  [conf {error :error :as task}]
  (µ/trace ::handler [:function "vxi11/handler"]
            (if error task
              (let [{error :error :as data} (query conf task)]
                (merge task (if error data (u/reshape data)))))))
