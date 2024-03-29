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

(defonce ctrls (atom {}))

(defn param->sign [h d] (keyword (str h d)))
  
(defn usr-ok? [usr] (and usr (nil? (.getString usr))))

(defn ctrl-alive? [ctrl] (and ctrl (.isConnected ctrl)))

(defn get-usr [] (VXI11UserFactory/create))

;; The `ctrl` establishes a link called `gpib0`. Via this connection a
;; device link `gpib0,4,0` is set up. While `(.disconnect dev)` closes
;; the device connection, `(.disconnect ctrl)` does not work. Therfore
;; the `ctrl` is kept.
(defn get-ctrl [{:keys [vxi-io-timeout vxi-lock-timeout]} usr host dev-name]
  (let [sign (param->sign host dev-name)
        ctrl (sign @ctrls)]
    (if-not (ctrl-alive? ctrl)
      (let [ctrl (VXI11Factory/create host dev-name)]
        (.setIoTimeout ctrl vxi-io-timeout)
        (.setLockTimeout ctrl vxi-lock-timeout)                     
        (.connect ctrl usr)
        (sign (swap! ctrls assoc sign ctrl)))
      ctrl)))

(defn dev-up [usr ctrl p-adr s-adr]
  (let [dev (.createDevice ctrl p-adr s-adr)]
    (.connect dev usr)
    dev))

(defn dev-down [usr ctrl dev]
  (.setREN ctrl usr false)
  (.disconnect dev))

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
  [{{:keys [read-buffer-size] :as conf} :vxi}
   {host :Host dev-name :DeviceName p-adr :PrimaryAddress s-adr :SecondaryAddress no-reply :NoReply :as task}]
  ;;(if-not (u/connectable? task)
  ;; {:error "can not connect"}
  (let [usr  (get-usr)
        ctrl (get-ctrl conf usr host dev-name)
        dev  (dev-up usr ctrl p-adr s-adr)
        bb   (byte-array read-buffer-size) 
        f    (fn [cmd]
               (when (seq cmd)    (.write dev usr (.getBytes cmd) (.length cmd)))
               (when-not no-reply (.read dev usr bb read-buffer-size))
               (bb->string bb))
        data (u/run f conf task)]
    (dev-down usr ctrl dev)
    data))

(comment
  (.connect (:e75416gpib0 @ctrls) (:e75416gpib0 @usrs)))

(defn handler
  "Handles VXI11 queries."
  [conf {error :error :as task}]
  (µ/trace ::handler [:function "vxi11/handler"]
            (if error task
              (let [{error :error :as data} (query conf task)]
                (merge task (if error data (u/reshape data)))))))
