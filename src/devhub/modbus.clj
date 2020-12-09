(ns devhub.modbus
  (:require [devhub.utils       :as u]
            [devhub.conf        :as c])
  (:import [com.digitalpetri.modbus.master
            ModbusTcpMasterConfig$Builder
            ModbusTcpMaster]))

(defn query
  [conf task]
  
  (let [config (.build (.setPort (ModbusTcpMasterConfig$Builder.) port))
        master (ModbusTcpMaster. config)]))

(defn safe
  [conf task]
  (let [{h :Host a :Address q :Quantity f :FunctionCode w :Wait r :Repeat n :NoReply} task]
    (when (and v a q f) (assoc task
                               :Value   (if (string? v) [v] v)
                               :Wait    (if w (u/number w) (:min-wait conf))
                               :Repeat  (if r (u/number r) (:repeat conf))
                               :NoReply (if n n false)))))
(defn handler
  "Handles Modbus queries.
  
  Example:
  ```clojure
  (def vc (:modbus (c/config)))
  (handler vc {:Device \"gpib0,8\" :Host \"e75465\" :Value \":meas:func\"})
  ```"
  [conf task]
  (if-let [task (safe conf task)]
    (if-let [data (query conf task)]
      {:data (u/meas-vec data)}
      {:error true :reason "no data"})
    {:error true :reason "missing <value>, <host> or <device>"}))



