(ns devhub.pp-scripts.utils-test
  (:require [clojure.test :refer :all]
            [devhub.pp-scripts.utils :refer :all]))

(deftest operable-i
  (testing "what have you (test genaration *blush*)"
    (is (= [true true true false false]
           (operable ["1" 1.234E-5 0 "a" :number]))
        "values")
    (is (= []
           (operable []))
        "empty")
    (is (= []
           (operable nil))
        "nil")
    (is (= []
           (operable ""))
        "empty string")
    (is (nil? (operable 1))
        " not seqable")))

(deftest operable-seq-i
  (testing "basics"
    (is (= ["1" 1.234E-5 0]
         (operable-seq  ["1" 1.234E-5 0 "a" :number]
                        [true true true false false]))
        "values")))

(deftest calc-seq-i
  (testing "basics"
    (is (= [1 1.234E-5 0]
         (calc-seq  ["1" 1.234E-5 0 "a" :number]
                        [true true true false false]))
        "values")
    (is (= []
           (let [fail ["failed-readout"]]
             (calc-seq fail (operable fail))))
        "values")))


(deftest calc-seq-ii
  (testing "intended use"
    (is (= [0 1 2 3]
           (let [v [0 "failed-readout" 1 2 "failed-readout" 3]
                 o (operable v)]
             (calc-seq v o)))
           "works")))
