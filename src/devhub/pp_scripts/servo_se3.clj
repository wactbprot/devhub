(ns devhub.pp-scripts.servo-se3
  (:require [devhub.pp-utils :as ppu]
            [devhub.utils    :as u]))

(def test-vec ["0" "-1" "-98" "3001" "" nil])

(defn extract
  [s]
  (when (string? s)
    (second (re-matches #"([-]?[0-9]*)" s))))

(defn meas-velo
  "Measures the Motor velocity. In case of a missing `v` (this happens
  with a ration of about 1/10000) `Servo_@motor_Stop.Bool` is set to
  false."
  [task]
  (let [motor  (get-in task [:PostScriptInput :Motor])
        min-v  (u/number (get-in task [:PostScriptInput :MinVelo]))
        cur-v  (Math/abs (u/number (extract (:_x task))))
        vkw    (keyword (str "Servo_" motor "_Velo"))
        skw    (keyword (str "Servo_" motor "_Stop"))
        mkw    (keyword (str "Servo_" motor "_Move"))]
    (merge task (if cur-v
                  {:ToExchange {vkw  {:Value cur-v :Unit "rpm"}
                                skw  {:Bool (> min-v cur-v)}
                                mkw  {:Bool (< min-v cur-v)}}}
                  {:ToExchange {vkw  {:Value 0 :Unit "rpm"}
                                skw  {:Bool false}
                                mkw  {:Bool true}}}))))
