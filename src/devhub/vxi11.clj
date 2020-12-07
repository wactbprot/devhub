(ns devhub.vxi11
  (:require [devhub.utils       :as u]
            [devhub.conf        :as c]
            [clojure.string     :as string])
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


(defn query
  [conf {h :host d :device pa :pri-adr sa :sec-adr} cmds wait repeat]
  (let [ba  (byte-array 100)
        ctrl (VXI11Factory/create h d)
        usr  (VXI11UserFactory/create)
        dev  (.createDevice ctrl pa sa)]
    (.connect ctrl usr)
    (.connect dev usr)
    (.remote dev usr)
    (.lock dev usr)
    (let [data (mapv (fn [_]
                       (let [v (mapv (fn [cmd]
                                       (let [bc (.getBytes cmd)
                                             lc (.length cmd)]
                                         (.write dev usr bc lc)
                                         (.read dev usr ba 100)
                                         (String. ba)))
                                     cmds)]
                         (Thread/sleep wait)
                         v))
                     (range repeat))]
      (.clear dev usr)
      (.unlock dev usr)
      (.local dev usr)
      (.disconnect dev)
      data)))

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
          conn    (assoc dev-adr :host h)]
      (if-let [data (query vxi-conf conn 
                           (if (string? v) [v] v)
                           (if w (u/number w) (:min-wait vxi-conf))
                           (if r (u/number r) (:repeat vxi-conf)))]
        {:data (u/meas-vec data)})
      {:error true :reason "no data"})
    {:error true :reason "missing <value>, <host> or <device>"}))
