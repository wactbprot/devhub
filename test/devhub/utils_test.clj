(ns devhub.utils-test
  (:require [clojure.test :refer :all]
            [devhub.utils :refer :all]))

(deftest reshape-i
  (testing "returns expected (i)"
    (is (map? (reshape {}))
        "map")
    (is (string? (:error (reshape nil)))
        "no data")
    (is (empty? (:_x (reshape [])))
        "nil")))

(deftest ascii-logo-i
  (testing "" (is (= "" (ascii-logo)) "")))
