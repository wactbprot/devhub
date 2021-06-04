(ns devhub.server-test
  (:require [devhub.config :as c]
            [clojure.test  :refer :all]
            [devhub.server :refer :all]
            [devhub.utils  :as u]))

(deftest start-stop-server-i
  (testing "server starts and stops at test port 9010"
    (let [c (assoc-in (c/config) [:server :port] 9010)]
      (is (function? (start c))
          "start up")
      (is (nil? (stop))
          "shut down"))))

(deftest thread-invalid-action-i
  (testing " returns error  (i)"
    (is (string? (:error (thread (c/config) {:Action "INVALID"})))
        "string")))

(deftest thread-tcp-i
  (testing " returns error  (i)"
    (is (string? (:error (thread (c/config) {:Action "TCP"})))
        "string")
    (is (string? (:error (thread (c/config) {:Action "TCP"
                                             :Host "invalid"
                                             :Port 10})))
        "string")
    (is (string? (:error (thread (c/config) {:Action "TCP"
                                          :Value "some"
                                          :Host "invalid"
                                          :Port 10})))
        "stub")
    (is (string? (:_x (thread (c/config) {:Action "TCP"
                                          :Value "some"
                                          :Host "invalid"
                                          :Port 10
                                          :stub true})))
        "stub")
    (is (vector? (:_x (thread (c/config) {:Action "TCP"
                                          :Value "some"
                                          :Repeat 2
                                          :Host "invalid"
                                          :Port 10
                                          :stub true})))
        "stub")))

(deftest thread-modbus-i
  (testing " returns error (i)"
    (is (string? (:error (thread (c/config) {:Action "MODBUS"})))
        "string")
    (is (string? (:error (thread (c/config) {:Action "MODBUS"
                                             :FunctionCode :ReadHoldingRegisters
                                             :Value [:no-value]
                                             :Address 0
                                             :Quantity 9
                                             :Host "invalid"})))
        "string")
    (is (vector? (:_x (thread (c/config) {:Action "MODBUS"
                                          :FunctionCode :ReadHoldingRegisters
                                          :Value [:no-value]
                                          :Address 0
                                          :Repeat 2
                                          :Quantity 9
                                          :Host "invalid"
                                          :stub true})))
        "stub")
    (is (vector? (:_x (thread (c/config) {:Action "MODBUS"
                                          :TaskName "VS_NEW_SE3-get_valve_pos"
                                          :FunctionCode :ReadHoldingRegisters
                                          :Value [:no-value]
                                          :Repeat 2
                                          :Address 0
                                          :Quantity 9
                                          :Host "invalid"
                                          :stub true})))
        "stub")
    (is (map? (:ToExchange (thread (c/config) {:Action "MODBUS"
                                               :TaskName "VS_NEW_SE3-get_valve_pos"
                                               :PostScript "vs_se3.valves"
                                               :FunctionCode :ReadHoldingRegisters
                                               :Value [:no-value]
                                               :Address 0
                                               :Quantity 9
                                               :Host "invalid"
                                               :stub true})))
        "stub with :PostScript")))

(deftest thread-vxi-i
  (testing " returns error (i)"
    (is (string? (:error (thread (c/config) {:Action "VXI11"})))
        "string")
    (is (string? (:error (thread (c/config) {:Action "VXI11"
                                             :Host "invalid"
                                             :Value "invalid"
                                             :Device "gpib0,9"})))
        "string")
    (is (string? (:_x (thread (c/config) {:Action "VXI11"
                                          :Host "invalid"
                                          :Value "some"
                                          :Device "gpib0,9"
                                          :stub true})))
        "stub")
    (is (vector? (:_x (thread (c/config) {:Action "VXI11"
                                          :Host "invalid"
                                          :Repeat 2
                                          :Value "some"
                                          :Device "gpib0,9"
                                          :stub true})))
        "stub")))

(deftest thread-execute-i
  (testing " returns error (i)"
    (is (string? (:error (thread (c/config) {:Action "EXECUTE"})))
        "string")
    (is (string? (:error (thread (c/config) {:Action "EXECUTE"
                                             :Cmd "invalid"})))
        "string")
    (is (string? (:_x (thread (c/config) {:Action "EXECUTE"
                                          :Cmd "invalid"
                                          :stub true})))
        "stub")
    (is (vector? (:_x (thread (c/config) {:Action "EXECUTE"
                                          :Repeat 2
                                          :Cmd "invalid"
                                          :stub true})))
        "stub")
    (is (string? (:_x (thread (c/config) {:Action "EXECUTE"
                                          :Cmd "ls"})))
        "real return value")
    (is (map? (:ToExchange (thread (c/config) {:Action "EXECUTE"
                                               :Cmd "ls"
                                               :PostScriptPy "ls-demo"
                                               :stub true})))
        "real return value")
    ))

