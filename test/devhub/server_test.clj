(ns devhub.server-test
  (:require [clojure.test  :refer :all]
            [devhub.server :refer :all]
            [devhub.utils  :as u]
            ))


(deftest start-stop-server-i
  (testing "server starts and stops at test port 9010"
    (let [c (assoc-in (u/config) [:server :port] 9010)]
      (is (function? (start c))
          "start up")
      (is (nil? (stop))
          "shut down"))))

(deftest thread-invalid-action-i
  (testing " returns error  (i)"
    (is (string? (:error (thread (u/config) {:Action "INVALID"} false)))
        "string")))

(deftest thread-tcp-i
  (testing " returns error  (i)"
    (is (string? (:error (thread (u/config) {:Action "TCP"} false)))
        "string")
    (is (string? (:error (thread (u/config) {:Action "TCP"
                                             :Host "invalid"
                                             :Port 10} false)))
        "string")
    (is (string? (:error (thread (u/config) {:Action "TCP"
                                          :Value "some"
                                          :Host "invalid"
                                          :Port 10} false)))
        "stub")
    (is (vector? (:_x (thread (u/config) {:Action "TCP"
                                          :Value "some"
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
                                             :Value "invalid"
                                             :Device "gpib0,9"} false)))
        "string")
    (is (vector? (:_x (thread (u/config) {:Action "VXI11"
                                          :Host "invalid"
                                          :Value "some"
                                          :Device "gpib0,9"} true)))
        "stub")))

(deftest thread-execute-i
  (testing " returns error (i)"
    (is (string? (:error (thread (u/config) {:Action "EXECUTE"} false)))
        "string")
    (is (string? (:error (thread (u/config) {:Action "EXECUTE"
                                             :Cmd "invalid"} false)))
        "string")
    (is (vector? (:_x (thread (u/config) {:Action "EXECUTE"
                                          :Cmd "invalid"} true)))
        "stub")
    (is (string? (:_x (thread (u/config) {:Action "EXECUTE"
                                          :Cmd "ls"} false)))
        "real return value")
    (is (map? (:ToExchange (thread (u/config) {:Action "EXECUTE"
                                          :Cmd "ls"
                                          :PostScriptPy "ls-demo"} false)))
        "real return value")
    ))

