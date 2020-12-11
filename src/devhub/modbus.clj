(ns devhub.modbus
  (:require [devhub.utils       :as u]
            [devhub.conf        :as c])
  (:import
   [com.intelligt.modbus.jlibmodbus Modbus]
   [com.intelligt.modbus.jlibmodbus.master ModbusMaster]
   [com.intelligt.modbus.jlibmodbus.master ModbusMasterFactory]
   [com.intelligt.modbus.jlibmodbus.tcp TcpParameters]))

(defn query
  "Executes the query depending on the `FunctionCode`.

  Example:
  ```clojure
  (def mc (:modbus (c/config)))
  (query mc {:Host \"e75446\" :Quantity 5 :Address 45407 :FunctionCode :ReadHoldingRegisters})

  (query mc {:Host \"e75480\" :Quantity 1 :Address 0 :FunctionCode :ReadInputRegisters})
  ```"
  [conf task]
  (let [{host   :Host
         fc     :FunctionCode
         val    :Value
         addr   :Address
         quant  :Quantity
         wait   :Wait
         repeat :Repeat
         norep  :NoReply} task
        slave-addr (:default-slave-address conf)
        param      (TcpParameters. host (:port conf) (:keep-alive conf))
        master     (ModbusMasterFactory/createModbusMasterTCP param)]
    (Modbus/setAutoIncrementTransactionId true)
    (.connect master)
    (let [data (condp = fc
                 :ReadHoldingRegisters (.readHoldingRegisters master slave-addr addr quant)
                 :ReadInputRegisters   (.readInputRegisters   master slave-addr addr quant)
                 :ReadCoils            (.readCoils            master slave-addr addr quant)
                 :ReadDiscreteInputs   (.readDiscreteInputs   master slave-addr addr quant)
                 :writeSingleRegister  (.writeSingleRegister  master slave-addr addr val))]
      (.disconnect master)
      data)))

(defn safe
  [conf task]
  (let [{h :Host a :Address q :Quantity fc :FunctionCode w :Wait r :Repeat n :NoReply} task]
    (when (and h a fc q) (assoc task
                                :FunctionCode (keyword fc)
                                :Address      (u/number a)
                                :Quantity     (u/number q)
                                :Wait         (if w (u/number w) (:min-wait conf))
                                :Repeat       (if r (u/number r) (:repeat conf))
                                :NoReply      (if n n false)))))

(defn handler
  "Handles Modbus queries.

  Example:
  ```clojure
   (def mc (:modbus (c/config)))
  (handler mc {:Host \"e75446\" :Quantity 5 :Address 45407 :FunctionCode \"ReadHoldingRegisters\"})
  ```"
  [conf task]
  (if-let [task (safe conf task)]
    (if-let [data (query conf task)]
      {:data (u/meas-vec data)}
      {:error true :reason "no data"})
    {:error true :reason "missing <value>, <host> or <device>"}))
