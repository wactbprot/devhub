(ns devhub.pp-scripts.mks670-test
    (:require [clojure.test :refer :all]
              [devhub.pp-scripts.mks670 :refer :all]))



(deftest ctrl-i
  (testing "Go"
    (let [task {:PostScriptInput {:Pressure_target {:Value 2 :Unit "mbar"} :Max_dev 0.005}
                :_x ["MEASURING  1.00" "MEASURING  1.00" "MEASURING  1.00"]
                :_t_start ["1638867427173" "1638867427310" "1638867427542"]
                :_t_stop ["1638867427210" "1638867427442" "1638867427579"]}
          task  (ctrl task)
          ok? (get-in task  [:ToExchange  :Filling_pressure_ok :Ready])
          dp  (get-in task  [:ToExchange  :Filling_pressure_dev :Value])]
      (is (false? ok?)
          "on lower pressure")
      (is (= 0.5 dp)
          "on lower pressure"))))

(deftest ctrl-ii
  (testing "Stop"
    (let [task {:PostScriptInput {:Pressure_target {:Value 1.001 :Unit "mbar"} :Max_dev 0.005}
                :_x ["MEASURING  1.00" "MEASURING  1.00" "MEASURING  1.00"]
                :_t_start ["1638867427173" "1638867427310" "1638867427542"]
                :_t_stop ["1638867427210" "1638867427442" "1638867427579"]}
          task  (ctrl task)
          ok? (get-in task  [:ToExchange  :Filling_pressure_ok :Ready])
          dp  (get-in task  [:ToExchange  :Filling_pressure_dev :Value])]
      (is (true? ok?)
          "on  pressure ok")
      (is (> 0.005 dp)
          "on pressure ok"))))


(deftest ctrl-iii
  (testing "Stop"
    (let [task {:PostScriptInput {:Pressure_target {:Value 0.95 :Unit "mbar"} :Max_dev 0.005}
                :_x ["MEASURING  1.00" "MEASURING  1.00" "MEASURING  1.00"]
                :_t_start ["1638867427173" "1638867427310" "1638867427542"]
                :_t_stop ["1638867427210" "1638867427442" "1638867427579"]}
          task  (ctrl task)
          ok? (get-in task  [:ToExchange  :Filling_pressure_ok :Ready])
          dp  (get-in task  [:ToExchange  :Filling_pressure_dev :Value])]
      (is (true? ok?)
          "on  over pressure")
      (is (= 0.0 dp)
          "on over pressure"))))
