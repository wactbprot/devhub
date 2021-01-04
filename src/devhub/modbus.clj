(ns devhub.modbus
  ^{:author "Wact B. Prot <wactbprot@gmail.com>"
    :doc "Handles MODBUS Actions."}
  (:require [devhub.utils           :as u]
            [devhub.safe            :as safe]
            [com.brunobonacci.mulog :as µ])
  (:import [com.intelligt.modbus.jlibmodbus Modbus]
           [com.intelligt.modbus.jlibmodbus.master ModbusMaster]
           [com.intelligt.modbus.jlibmodbus.master ModbusMasterFactory]
           [com.intelligt.modbus.jlibmodbus.tcp TcpParameters]
           [java.net InetAddress]))

(defn query
  "Handles Modbus queries. Executes the query depending on the `FunctionCode`.

  Example:
  ```clojure
  (def mc (:modbus (u/config)))
  (query mc {:Host \"e75446\" :Quantity 5 :Address 45407 :FunctionCode :ReadHoldingRegisters})

  (query mc {:Host \"e75480\" :Quantity 1 :Address 0 :FunctionCode :ReadInputRegisters})
  ```"
  [{conf :modbus} task]
  (let [{host :Host fc :FunctionCode addr :Address q :Quantity} task]
    (µ/log ::query :req-id (:req-id task) :Host host :Address addr)
    (let [ok-or-err (try (InetAddress/getByName host)
                         (catch  Exception e
                           (µ/log ::query :error "connection error" :req-id (:req-id task))
                           {:error "can not connect to host"}))]
      (if (:error ok-or-err)
        ok-or-err
        (let [param  (TcpParameters. host (:port conf) (:keep-alive conf))
              master (ModbusMasterFactory/createModbusMasterTCP param)
              s-addr (:default-slave-address conf)]
          (Modbus/setAutoIncrementTransactionId true)
          (.connect master)
          (let [f (condp = fc
                  :ReadHoldingRegisters (fn [x] (.readHoldingRegisters master s-addr addr q)) 
                  :ReadInputRegisters   (fn [x] (.readInputRegisters   master s-addr addr q))
                  :ReadCoils            (fn [x] (.readCoils            master s-addr addr q)) 
                  :ReadDiscreteInputs   (fn [x] (.readDiscreteInputs   master s-addr addr q))
                  :writeSingleRegister  (fn [x] (.writeSingleRegister  master s-addr addr x)))
                data (u/run f conf task)]
            (.disconnect master)
            data))))))
