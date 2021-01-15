(ns devhub.utils-test
  (:require [clojure.test :refer :all]
            [devhub.utils :refer :all]))

(deftest meas-vec-i
  (testing "returns expected (i)"
    (is (map? (meas-vec {}))
        "map")
    (is (string? (:error (meas-vec nil)))
        "no data")
    (is (empty? (:_x (meas-vec [])))
        "nil")))

(deftest ascii-logo-i
  (testing "" (is (= "" (ascii-logo)) "")))
