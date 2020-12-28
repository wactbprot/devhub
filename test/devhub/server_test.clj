(ns devhub.server-test
  (:require [clojure.test  :refer :all]
            [devhub.server :refer :all]
            [devhub.utils  :as u]
            ))

(deftest thread-tcp-i
  (testing " returns error  (i)"
    (is (string? (:error (thread (u/config) {:Action "TCP"} false)))
        "string")
    (is (string? (:error (thread (u/config) {:Action "TCP"
                                             :Host "invalid"
                                             :Port 10} false)))
        "string")
    (is (vector? (:_x (thread (u/config) {:Action "TCP"
                                             :Host "invalid"
                                             :Port 10} true)))
        "stub")))

(deftest thread-modbus-i
  (testing " returns error (i)"
    (is (string? (:error (thread (u/config) {:Action "MODBUS"} false)))
        "string")
    (is (string? (:error (thread (u/config) {:Action "MODBUS"
                                             :FunctionCode :ReadHoldingRegisters
                                             :Value [:no-value]
                                             :Address 0
                                             :Quantity 9
                                             :Host "invalid"} false)))
        "string")
    (is (vector? (:_x (thread (u/config) {:Action "MODBUS"
                                             :FunctionCode :ReadHoldingRegisters
                                             :Value [:no-value]
                                             :Address 0
                                             :Quantity 9
                                             :Host "invalid"} true)))
        "stub")))

(deftest thread-vxi-i
  (testing " returns error (i)"
    (is (string? (:error (thread (u/config) {:Action "VXI11"} false)))
        "string")
    (is (string? (:error (thread (u/config) {:Action "VXI11"
                                             :Host "invalid"
                                             :Device "gpib0,9"} false)))
        "string")
    (is (vector? (:_x (thread (u/config) {:Action "VXI11"
                                             :Host "invalid"
                                             :Device "gpib0,9"} true)))
        "stub")))
