(ns devhub.modbus
  (:require [devhub.utils :as u])
  (:import
   [com.intelligt.modbus.jlibmodbus Modbus]
   [com.intelligt.modbus.jlibmodbus.master ModbusMaster]
   [com.intelligt.modbus.jlibmodbus.master ModbusMasterFactory]
   [com.intelligt.modbus.jlibmodbus.tcp TcpParameters]
   [java.net InetAddress]))

(defn query
  "Executes the query depending on the `FunctionCode`.

  Example:
  ```clojure
  (def mc (:modbus (u/config)))
  (query mc {:Host \"e75446\" :Quantity 5 :Address 45407 :FunctionCode :ReadHoldingRegisters})

  (query (u/config) {:Host \"e75480\" :Quantity 1 :Address 0 :FunctionCode :ReadInputRegisters})
  ```"
  [conf task]
  (let [{host   :Host
         fc     :FunctionCode
         value  :Value
         addr   :Address
         quant  :Quantity
         wait   :Wait
         rep    :Repeat
         norep  :NoReply} task
        slave-addr (:default-slave-address conf)
        ip     (InetAddress/getByName host)
        param      (TcpParameters. host (:port conf) (:keep-alive conf))
        master     (ModbusMasterFactory/createModbusMasterTCP param)]
    (Modbus/setAutoIncrementTransactionId true)
    (.connect master)
    (let [t0  (u/ms)
          res (condp = fc
                :ReadHoldingRegisters (.readHoldingRegisters master slave-addr addr quant) 
                :ReadInputRegisters   (.readInputRegisters   master slave-addr addr quant)
                :ReadCoils            (.readCoils            master slave-addr addr quant) 
                :ReadDiscreteInputs   (.readDiscreteInputs   master slave-addr addr quant)
                :writeSingleRegister  (.writeSingleRegister  master slave-addr addr value))
          t1  (u/ms)]
      (.disconnect master)
      (u/add-times {:_x res} t0 t1))))


(defn safe
  [conf task]
  (let [{h :Host a :Address q :Quantity fc :FunctionCode w :Wait r :Repeat} task]
    (when (and h a fc q) (assoc task
                                :FunctionCode (keyword fc)
                                :Address      (u/number a)
                                :Quantity     (u/number q)
                                :Wait         (if w (u/number w) (:min-wait conf))
                                :Repeat       (if r (u/number r) (:repeat conf))))))

(defn handler
  "Handles Modbus queries.

  Example:
  ```clojure
  (handler (u/config) {:Host \"e75446\" :Quantity 5 :Address 45407 :FunctionCode \"ReadHoldingRegisters\"})
  ```"
  [conf task]
  (if (:on (:stub conf))
    (u/stub-response conf task)
    (if-let [task (safe (:modbus conf) task)]
      (if-let [data (query (:modbus conf) task)]
        {:data (u/meas-vec data)}
        {:error true :reason "no data"})
      {:error true :reason "missing <functioncode>, <host>, <address> or <quantity>"})))