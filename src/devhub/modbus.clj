(ns devhub.modbus
  (:require [devhub.utils           :as u]
            [devhub.safe            :as safe]
            [com.brunobonacci.mulog :as µ])
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

  (query mc {:Host \"e75480\" :Quantity 1 :Address 0 :FunctionCode :ReadInputRegisters})
  ```"
  [conf task]
  (let [{host :Host fc :FunctionCode addr :Address q :Quantity} task
        s-addr  (:default-slave-address conf)
        ip      (InetAddress/getByName host)
        param   (TcpParameters. host (:port conf) (:keep-alive conf))
        master  (ModbusMasterFactory/createModbusMasterTCP param)]
    (Modbus/setAutoIncrementTransactionId true)
    (.connect master)
    (let [f    (condp = fc
                 :ReadHoldingRegisters (fn [x] (.readHoldingRegisters master s-addr addr q)) 
                 :ReadInputRegisters   (fn [x] (.readInputRegisters   master s-addr addr q))
                 :ReadCoils            (fn [x] (.readCoils            master s-addr addr q)) 
                 :ReadDiscreteInputs   (fn [x] (.readDiscreteInputs   master s-addr addr q))
                 :writeSingleRegister  (fn [x] (.writeSingleRegister  master s-addr addr x)))
          data (u/run f conf task)]
      (.disconnect master)
      data)))

(defn handler
  "Handles Modbus queries.

  Example:
  ```clojure
  (handler (u/config) {:Host \"e75446\" :Quantity 5 :Address 45407 :FunctionCode \"ReadHoldingRegisters\"})
  ```"
  [{conf :modbus} task]
  (if-let [task (safe/modbus conf task)]
    (if-let [data (try         
                    (query conf task)
                    (catch Exception e
                      (µ/log ::exec :exception e :status :failed :req-id (:req-id task))
                      {:error (str "caught exception: " (.getMessage e))}))]
      (u/meas-vec data)
      {:error true :reason "no data"})
    {:error true :reason "missing <functioncode>, <host>, <address> or <quantity>"}))
