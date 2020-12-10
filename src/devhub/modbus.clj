(ns devhub.modbus
  (:require [devhub.utils       :as u]
            [devhub.conf        :as c])
  (:import [com.digitalpetri.modbus.master
            ModbusTcpMasterConfig$Builder
            ModbusTcpMaster]))

(defn query
  [conf task]
  (let [{host :Host addr :Address cmds :Value wait :Wait repeat :Repeat norep :NoReply} task
        config (.build (.setPort (ModbusTcpMasterConfig$Builder. host) addr))
        master (ModbusTcpMaster. config)]
    (.connect master)
    (prn (.getMeanRate  (.getResponseTimer master)))
    (Thread/sleep 100)
    (.disconnect master)))

(defn safe
  [conf task]
  (let [{h :Host a :Address  f :FunctionCode w :Wait r :Repeat n :NoReply} task]
    (when (and h a f) (assoc task
                             :Address (u/number a)
                             :Wait    (if w (u/number w) (:min-wait conf))
                             :Repeat  (if r (u/number r) (:repeat conf))
                             :NoReply (if n n false)))))
(defn handler
  "Handles Modbus queries.
  
  Example:
  ```clojure
   (def mc (:modbus (c/config)))
  (handler mc {:Host \"172.30.56.46\" :Address 45407 :FunctionCode \"ReadHoldingRegisters\"})
  ```"
  [conf task]
  (if-let [task (safe conf task)]
    (if-let [data (query conf task)]
      {:data (u/meas-vec data)}
      {:error true :reason "no data"})
    {:error true :reason "missing <value>, <host> or <device>"}))



