(ns devhub.modbus
  ^{:author "wactbprot"
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
  "Executes the query depending on the `FunctionCode`.

  Example:
  ```clojure
  (def mc (:modbus (u/config)))
  (query mc {:Host \"e75446\" :Quantity 5 :Address 45407 :FunctionCode :ReadHoldingRegisters})

  (query mc {:Host \"e75480\" :Quantity 1 :Address 0 :FunctionCode :ReadInputRegisters})
  ```"
  [conf task]
  (let [{host :Host fc :FunctionCode addr :Address q :Quantity} task
        ok-or-err (try
                    (InetAddress/getByName host)
                    (catch  Exception e
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
          data)))))

(defn handler
  "Handles Modbus queries.
  
  Example:
  ```clojure
  (handler (u/config) {:Host \"e75446\" :Quantity 5 :Address 45407 :FunctionCode \"ReadHoldingRegisters\"})
  ```"
  [{conf :modbus} task]
  (if-let [task (safe/modbus conf task)]
    (let [data-or-err (query conf task)]
      (if (:error data-or-err)
        (let [error   data-or-err
              err-msg (:error error)]
          (µ/log ::handler :error err-msg :req-id (:req-id task))
          error)
        (let [data (u/meas-vec data-or-err)]
          (µ/log ::handler :data data  :req-id (:req-id task))
          data)))
    {:error  "missing <functioncode>, <host>, <address> or <quantity>"}))