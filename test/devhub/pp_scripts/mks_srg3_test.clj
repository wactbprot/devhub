(ns devhub.pp-scripts.mks-srg3-test
    (:require [clojure.test :refer :all]
              [devhub.pp-scripts.mks-srg3 :refer :all]))


(def x [" 2.3274E-07 " "> 2.3325E-07 " "> 2.3325E-07 "
        "> 2.3325E-07 " "> 2.3265E-07 " ">>> 2.3265E-07 "
        "> 2.3265E-07 "])

(def t0 ["1660832233286" "1660832243337" "1660832253388"
         "1660832263439" "1660832273490" "1660832283541"
         "1660832293592"])

(def t1 ["1660832233337" "1660832243388" "1660832253439"
         "1660832263490" "1660832273541" "1660832283592"
         "1660832293643"])

  (deftest read-out-i
  (testing "basics"
    (is (nil? (first (mapv extract x)))
           "skips 1st val"))))

