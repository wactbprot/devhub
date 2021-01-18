(ns devhub.modbus
  ^{:author "Wact B. Prot <wactbprot@gmail.com>"
    :doc "Handles MODBUS Actions."}
  (:require [devhub.utils           :as u]
            [devhub.safe            :as safe]
            [com.brunobonacci.mulog :as µ])
  (:import [com.intelligt.modbus.jlibmodbus Modbus]
           [com.intelligt.modbus.jlibmodbus.master ModbusMaster]
           [com.intelligt.modbus.jlibmodbus.master ModbusMasterFactory]
           [com.intelligt.modbus.jlibmodbus.tcp TcpParameters]))

(defn I->v
  "Returns a vector of integers made from the input `[I` type."
  [I]
  (mapv int I))

(defn query
  "Executes the query depending on the `FunctionCode`.
  
  Example:
  ```clojure
  (def c (u/config))

  ;; valves
  (def t {:Host \"e75446\" :Quantity 9 :Address 45407
         :FunctionCode :ReadHoldingRegisters
         :Value [:no-value] :Wait 10 :Repeat 1})
  (query c t)
  
  ;; switches
  (def t {:Host \"e75446\" :Quantity 9 :Address 45395
         :FunctionCode :ReadHoldingRegisters
         :Value [:no-value] :Wait 10 :Repeat 1})
  (query c t)

  ;; cdgs 
  (def t {:Host \"e75480\" :Quantity 64 :Address 0
           :FunctionCode :ReadInputRegisters
            :Value [:no-value] :Wait 10 :Repeat 1})
  (query c t)
  ```"
  [{conf :modbus} task]
  (if-not (u/connectable? task)
    {:error "can not connect"}
    (let [{host :Host fc :FunctionCode addr :Address q :Quantity} task
          s-addr (:default-slave-address conf)
          param  (TcpParameters. host (:port conf) (:keep-alive conf))
          master (ModbusMasterFactory/createModbusMasterTCP param)]
      (Modbus/setAutoIncrementTransactionId true)
      (.connect master)
      (let [f (condp = fc
                :ReadHoldingRegisters (fn [x] (I->v (.readHoldingRegisters master s-addr addr q))) 
                :ReadInputRegisters   (fn [x] (I->v (.readInputRegisters   master s-addr addr q)))
                :ReadCoils            (fn [x] (I->v (.readCoils            master s-addr addr q))) 
                :ReadDiscreteInputs   (fn [x] (I->v (.readDiscreteInputs   master s-addr addr q)))
                :writeSingleRegister  (fn [x] (.writeSingleRegister        master s-addr addr x)))
            data (u/run f conf task)]
        (.disconnect master)
        data))))

(defn handler
  "Handles Modbus queries. "
  [conf task]
  (if (:error task) task
      (let [{host :Host fc :FunctionCode addr :Address q :Quantity req-id :req-id} task
            _    (µ/log ::query :req-id req-id :Host host :Address addr)
            data (query conf task)]
        (merge task (if (:error data)
                      (let [msg (:error data)]
                        (µ/log ::query :error msg :req-id req-id)
                        data)
                      (let [msg "received data"]
                        (µ/log ::query :message msg :req-id req-id)
                        (u/reshape data)))))))
