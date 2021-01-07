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

(defn I->v
  "Returns a vector of integers made from the input `[I` type."
  [i]
  (mapv int i))

(defn query
  "Handles Modbus queries. Executes the query depending on the `FunctionCode`.

  Example:
  ```clojure
  (def c (u/config))
  ;; valves
  (def t {:Host \"e75446\" :Quantity 9 :Address 45407
         :FunctionCode :ReadHoldingRegisters
         :Value [:no-value] :Wait 10 :Repeat 1})
  (query c t)
  ;; =>
  ;; {:_x [80 0 17 0 16 0 21 0 3],
  ;;  :_t_start 1609917641895
  ;;  :_t_stop 1609917641898
  ;;  :_dt 3}

  ;; switches
  (def t {:Host \"e75446\" :Quantity 9 :Address 45395
         :FunctionCode :ReadHoldingRegisters
         :Value [:no-value] :Wait 10 :Repeat 1})
  (query c t)

  (def t {:Host \"e75480\" :Quantity 64 :Address 0
           :FunctionCode :ReadInputRegisters
            :Value [:no-value] :Wait 10 :Repeat 1})
  (query c t)
   ;; =>
   ;; {:_x 
   ;; [60  44  0  1  60  -25  0  1  60  113  0  1  60  78  0  1
   ;;  60  -56  0  1  60  78  0  1  0  0  5  1  0  0  5  1  0
   ;;  0  5  1  0  0  5  1  0  0  5  1  0  0  5  1
   ;;  60  33  0  1  0  0  5  1  0  0  5  1  0  0  5  1  0
   ;;  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0
   ;;  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0
   ;;  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0
   ;;  0  0  0  0  0  0  0  0  0]
   ;;  :_t_start 1609917864255
   ;;  :_t_stop 1609917864257
   ;;  :_dt 2}
  ```"
  [{conf :modbus} task]
  (let [{host :Host fc :FunctionCode addr :Address q :Quantity} task]
    (µ/log ::query :req-id (:req-id task) :Host host :Address addr)
    (let [ok-or-err (try (InetAddress/getByName host)
                         (catch  Exception e
                           (µ/log ::query :error "connection error" :req-id (:req-id task))
                           {:error "can not connect to host"}))]
      (if (:error ok-or-err) ok-or-err
          (let [param  (TcpParameters. host (:port conf) (:keep-alive conf))
                master (ModbusMasterFactory/createModbusMasterTCP param)
                s-addr (:default-slave-address conf)]
            (Modbus/setAutoIncrementTransactionId true)
            (.connect master)
            (let [f (condp = fc
                      :ReadHoldingRegisters (fn [x] (I->v (.readHoldingRegisters master s-addr addr q))) 
                      :ReadInputRegisters   (fn [x] (I->v (.readInputRegisters   master s-addr addr q)))
                      :ReadCoils            (fn [x] (I->v (.readCoils            master s-addr addr q))) 
                      :ReadDiscreteInputs   (fn [x] (I->v (.readDiscreteInputs   master s-addr addr q)))
                      :writeSingleRegister  (fn [x] (.writeSingleRegister  master s-addr addr x)))
                  data (u/run f conf task)]
              (.disconnect master)
              data))))))
