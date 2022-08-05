(ns devhub.pp-scripts.vat-dosing-valve-test
    (:require [clojure.test :refer :all]
              [devhub.pp-scripts.vat-dosing-valve :refer :all]))

(deftest int->pos-str-i
  (testing "conversion basics"
    (is (= "R:000001\r\n" (int->pos-str 1))
        "int val")
    (is (= "R:000001\r\n" (int->pos-str 1.1))
        "float val")
    (is (= "R:000001\r\n" (int->pos-str "1"))
        "float val")
    (is (= "R:000001\r\n" (int->pos-str "1.1"))
        "float val")
    (is (= "R:000000\r\n" (int->pos-str "a"))
        "float val")))


(deftest pos-str->int-i
  (testing "conversion basics"
    (is (= 0 (pos-str->int "i:3800000000"))
        "int val")
    (is (= 10 (pos-str->int "i:3800000010"))
        "int val")))
