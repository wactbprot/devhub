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

(deftest mean-i
  (testing "intended use"
    (is (= 2 (mean [0 1 2 3 4]))
        "works")
    (is (= 2.0 (mean [0.0 1.0 2.0 3.0 4.0]))
        "works")
    (is (nil? (mean nil))
        "works")
    (is (nil? (mean []))
           "works")))

(deftest sd-i
  (testing "R comparison: diff. is smaller than 1e-9
   > n <- rnorm(10)
   > n
    [1] -0.2087914 -0.4030431 -1.3263779 -1.1265410 -2.2380873 -0.5273883
    [7]  0.2371203 -1.2621084  0.2957844 -0.7099522
   > sd(n)
   [1] 0.7801604"
    (is (> 1e-9 
           (- 0.7801604
              (sd [-0.2087914 -0.4030431 -1.3263779 -1.1265410 -2.2380873 -0.5273883
                   0.2371203 -1.2621084  0.2957844 -0.7099522])))
           "works")))
