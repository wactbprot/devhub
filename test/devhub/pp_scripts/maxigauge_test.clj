(ns devhub.pp-scripts.maxigauge-test
    (:require [clojure.test :refer :all]
              [devhub.pp-scripts.maxigauge :refer :all]))


(def v ["" "0,-2.4342E+00"])

(def vv [""
        "0,-2.4342E+00"
        ""
        "0,+9.4771E-05"
        ""
        "0,+1.4055E-03"
        ""
        "0,-1.4192E-04"
        ""
        "0,+4.5151E-04"])

(def i {:CH1 {:Reservoir 3, :Fullscale 1000, :Unit "mbar"},
        :CH2 {:Reservoir 3, :Fullscale 10, :Unit "mbar"},
        :CH3 {:Reservoir 4, :Fullscale 10, :Unit "mbar"},
        :CH4 {:Reservoir 4, :Fullscale 0.1, :Unit "mbar"},
        :CH5 {:Reservoir 5, :Fullscale 10, :Unit "mbar"},
        :CH6 {:Reservoir 5, :Fullscale 0.1, :Unit "mbar"}})


(deftest extract-value-i
  (testing "extract-value basics"
    (is (= 4.5151E-04 (extract-value "0,+4.5151E-04"))
        "simple val")
    (is (= 4.5151E-04 (extract-value "1,+4.5151E-04"))
        "underrange values")
    (is (nil? (extract-value "+4.5151E-04"))
        "wrong value syntax gives nil")
    (is (nil? (extract-value "+."))
        "wrong value syntax gives nil")))


(deftest last-pressure-value-i
   (testing "pressure value basics"
    (is (= -2.4342 (last-pressure-value ["" "0,-2.4342E+00"]))
        "simple val")
    (is (= -2.4342 (last-pressure-value ["0,-2.4342E+00"]))
        "without ack")
    (is (= -2.4342 (last-pressure-value ["1,-2.4342E+00"]))
        "without ack, underrange")
    (is (= 4.5151E-04 (last-pressure-value ["" "0,-2.4342E+00" 
                                            "" "0,+9.4771E-05" 
                                            "" "0,+1.4055E-03" 
                                            "" "0,-1.4192E-04" 
                                            "" "0,+4.5151E-04"]))
        "last")
    (is (nil? (last-pressure-value ["-2.4342E+00"]))
        "maleformed i")
    (is (nil? (last-pressure-value ["1,  E+00"]))
        "maleformed ii")))
 

(deftest pressure-safe?-i
   (testing "pressure safe basics (pressure-safe? current-pressure target-pressure)"
     (is (true? (pressure-safe? 1 1))
         "integer")
     (is (true? (pressure-safe? 1. 1.))
         "float")))
