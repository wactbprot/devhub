(ns devhub.pp-scripts.servo-se3
  (:require [devhub.pp-utils :as ppu]
            [clojure.string  :as string]
            [devhub.utils    :as u]))

(def test-vec ["0" "-1" "-98" "3001" "" nil])

(defn extract [s] (when (string? s) (second (re-matches #"([-]?[0-9]*)" s))))

(defn meas-velo
  "Measures the Motor velocity. In case of a missing `v` (this happens
  with a ration of about 1/10000) `Servo_@motor_Stop.Bool` is set to
  false."
  [task]
  (let [motor  (get-in task [:PostScriptInput :Motor])
        min-v  (u/number (get-in task [:PostScriptInput :MinVelo]))
        cur-v  (u/number (extract (:_x task)))
        vkw    (keyword (str "Servo_" motor "_Velo"))
        skw    (keyword (str "Servo_" motor "_Stop"))
        mkw    (keyword (str "Servo_" motor "_Move"))]
    (merge task (if cur-v
                  {:ToExchange {vkw  {:Value cur-v :Unit "rpm"}
                                skw  {:Bool (> min-v (Math/abs cur-v))}
                                mkw  {:Bool (< min-v (Math/abs cur-v))}}}
                  {:Retry true
                   :ToExchange {skw  {:Bool false}
                                mkw  {:Bool true}}}))))

(defn resp-ok
  [task]
  (let [motor (get-in task [:PostScriptInput :Motor])
        kw    (keyword (str "Servo_" motor "_Ini"))
        res    (:_x task)]
    (merge task (if (and (string? res) (string/includes?  res "OK"))
                  {:ToExchange {kw {:Bool true}}}
                  {:Retry true
                   :ToExchange {kw {:Bool false}}}))))

(defn get-pos
  [task]
  (let [motor (get-in task [:PostScriptInput :Motor])
        kw    (keyword (str "Servo_" motor "_Pos"))
        pos   (u/number (extract (:_x task)))]
    (merge task (if (number? pos)
                  {:ToExchange {kw {:Value pos :Unit "step"}}}
                  {:Retry true
                   :ToExchange {kw false }}))))

(defn set-velo
  [task]
  (let [motor  (get-in task [:PostScriptInput :Motor])
        velo   (u/number (get-in task [:PostScriptInput :Velo]))
        skw    (keyword (str "Servo_" motor "_Stop"))
        mkw    (keyword (str "Servo_" motor "_Move"))
        res    (:_x task)]
    (merge task (if (and (string? res) (string/includes? res "OK"))
                  {:ToExchange {skw  {:Bool (zero? velo)}
                                mkw  {:Bool (not (zero? velo))}}}
                  {:Retry true
                   :ToExchange {skw  {:Bool (not (zero? velo))}
                                mkw  {:Bool (zero? velo)}}}))))
