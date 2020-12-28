(ns devhub.server-test
  (:require [clojure.test  :refer :all]
            [devhub.server :refer :all]
            [devhub.utils  :as u]
            ))

(deftest thread-tcp-i
  (testing " returns error (i)"
    (is (string? (:error (thread (u/config) {:Action "TCP"} false)))
        "nil .")))

(deftest thread-modbus-i
  (testing " returns error (i)"
    (is (string? (:error (thread (u/config) {:Action "MODBUS"} false)))
        "nil .")))

(deftest thread-vxi-i
  (testing " returns error (i)"
    (is (string? (:error (thread (u/config) {:Action "VXI"} false)))
        "nil .")))
