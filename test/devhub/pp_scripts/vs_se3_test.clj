(ns devhub.pp-scripts.vs_se3-test
  (:require [clojure.test :refer :all]
            [devhub.pp-scripts.vs_se3 :refer :all]))

(deftest switches-i
  (testing " returns nil (i)"
    (let [arr [1025, 0, 21760, 0, 0, 0, 1024, 0, 7]
          res (switches {} {:_x arr})]
      (is (map? (:ToExchange res))
          "ToExchange map")
      (is (= arr  (get-in res [:ToExchange :registers]))
          "registers"))))

(deftest valves-i
  (testing "ok"
    (let [arr [1025, 0, 21760, 0, 0, 0, 1024, 0, 7]
          res (valves {} {:_x arr})]
      (is (map? (:ToExchange res))
          "ToExchange map")
      (is (= arr  (get-in res [:ToExchange :registers]))
          "registers"))))
