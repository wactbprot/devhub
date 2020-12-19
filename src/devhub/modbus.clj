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
         cmds   :Value
         addr   :Address
         q      :Quantity
         wait   :Wait
         rep    :Repeat
         norep  :NoReply} task
        s-addr (:default-slave-address conf)
        ip     (InetAddress/getByName host)
        param      (TcpParameters. host (:port conf) (:keep-alive conf))
        master     (ModbusMasterFactory/createModbusMasterTCP param)]
    (Modbus/setAutoIncrementTransactionId true)
    (.connect master)
    (let [f    (condp = fc
                 :ReadHoldingRegisters (fn [x] (.readHoldingRegisters master s-addr addr q)) 
                 :ReadInputRegisters   (fn [x] (.readInputRegisters   master s-addr addr q))
                 :ReadCoils            (fn [x] (.readCoils            master s-addr addr q)) 
                 :ReadDiscreteInputs   (fn [x] (.readDiscreteInputs   master s-addr addr q))
                 :writeSingleRegister  (fn [x] (.writeSingleRegister  master s-addr addr x)))
          data    (u/run (fn [cmd] (let [t0  (u/ms)
                                         res (f cmd)]
                                     (u/add-times {:_x res} t0 (u/ms)))) cmds wait rep)]
      (.disconnect master)
      data)))

(defn safe
  [conf task]
  (let [{h :Host a :Address q :Quantity fc :FunctionCode w :Wait r :Repeat v :Value} task]
    (when (and h a fc q) (assoc task
                                :FunctionCode (keyword fc)
                                :Value        (cond
                                                (nil? v)    [:no-value]
                                                (string? v) [v]
                                                (vector? v) v)
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
  [{conf :modbus} task]
  (if-let [task (safe conf task)]
    (if-let [data (query conf task)]
      {:data (u/meas-vec data)}
      {:error true :reason "no data"})
    {:error true :reason "missing <functioncode>, <host>, <address> or <quantity>"}))