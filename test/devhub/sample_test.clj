(ns devhub.sample-test
  (:require [clojure.test  :refer :all]
            [devhub.utils  :as u]
            [devhub.sample :refer :all]))

(def d2 {:_x [[1025 0 21760 0 1 0 1024 0 7] [1025 0 21760 0 1 0 1024 0 7]]
         :_t_start ["1609830521637" "1609830521653"]
         :_t_stop ["1609830521638" "1609830521653"]})


(def d1 {:_x [1025 0 21760 0 1 0 1024 0 7]
         :_t_start "1609830521637"
         :_t_stop "1609830521638"})

(def ar {:missing ["foo" "123" "###########" "2e-3 Pa" "MEAS 23.1 C"]
         :VS_SE3-get-valves-pos [[1025 0 21760 0 0 0 1024 0 7]
                                 [1025 0 21760 0 1 0 1024 0 7]
                                 [1025 0 21760 0 1024 0 1024 0 7]]
         :VS_SE3-get-switches-pos [[1025 0 21760 0 0 0 1024 0 7]]})


(def kw :VS_SE3-get-valves-pos)

(deftest insert-i
    (is (= 5 (count (kw (insert (u/config) ar kw d2))))
        "result in 4 entries")
  (testing "inserts vectors (i)"
    (is (= 4 (count (kw (insert (u/config) ar kw d1))))
        "result in 4 entries")))
