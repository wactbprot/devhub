(ns devhub.vxi11
  (:require [devhub.utils       :as u]
            [devhub.conf        :as c]
            [clojure.string     :as string])
  (:import  [jvxi11 VXI11Factory VXI11UserFactory]))

(defn parse-gpib-str
  [s]
  (let [v (re-matcher #"gpib([0-9]*),([0-9]*)" s)]
    (if (empty? v)
      {:device "inst0" :pri-adr -1 :sec-adr -1} 
      {:device "inst0"
       :pri-adr (u/number (nth v 1))
       :sec-adr (u/number (nth v 2))})))

(defn parse-inst-str [s] {:device s :pri-adr -1 :sec-adr -1})
      
(defn parse-device-str
  [s]
  (cond
    (string/starts-with? s "inst") (parse-inst-str s)
    (string/starts-with? s "gpib") (parse-gpib-str s)
    :else (throw "imprement me")))
  
(defn query
  [h d v w r]

  (let [adr  (parse-device-str d)
        ctrl (VXI11Factory/create  h (:device adr))
        usr  (VXI11UserFactory/create)
        rval1 (.connect ctrl usr)
        dev  (.createDevice ctrl (:pri-adr adr) (:sec-adr adr))
        rval2 (.connect dev usr)
        rval3 (.remote dev usr)
        rval4 (.write dev usr (.getBytes (first v)) (.length v))
        rval5 (.read dev (byte-array 10) 50)
        ]
    (prn rval1)
    (prn rval2)
    (prn rval3)
    (prn rval4)
    (prn rval5)
    (.disconnect dev)))

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
    (if-let [data (query h d
                         (if (string? v) [v] v)
                         (if w (u/number w) (:min-wait vxi-conf))
                         (if r (u/number r) (:repeat vxi-conf)))]
      {:data (u/meas-vec data)}
      {:error true :reason "no data"})
    {:error true :reason "missing <value>, <host> or <device>"}))
