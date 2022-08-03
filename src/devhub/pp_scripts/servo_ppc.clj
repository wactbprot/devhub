(ns devhub.pp-scripts.servo-ppc
  ^{:author "Thomas Bock <wactbprot@gmail.com>"
    :doc "Processing for PPC servo motors."}
  (:require [devhub.pp-utils :as ppu]
            [clojure.string :as string]
            [devhub.utils :as u]))


(defn extract [s] (when (string? s)  (second (re-matches #"([-]?[0-9]*)" s))))

(defn meas-velo
  "Measures the Motor velocity. In case of a missing `v` (this happens
  with a ration of about 1/10000) `Servo_PPC_Stop.Bool` is set to
  false."
  [{x :_x :as task}]
  
  (let [min-velo 5 ; rpm
        cur-velo (-> x extract u/number)]
    (merge task (if cur-velo
                  {:ToExchange {:Servo_PPC_Velo  {:Value cur-velo :Unit "rpm"}
                                :Servo_PPC_Stop  {:Bool (> min-velo (Math/abs cur-velo))}
                                :Servo_PPC_Move  {:Bool (< min-velo (Math/abs cur-velo))}}}
                  {:Retry true
                   :ToExchange {:Servo_PPC_Stop  {:Bool false}
                                :Servo_PPC_Move  {:Bool true}}}))))


(defn get-pos
  "Measures the Motor velocity. In case of a missing `v` (this happens
  with a ration of about 1/10000) `Servo_PPC_Stop.Bool` is set to
  false."
  [{x :_x :as task}]
  (let [pos (u/number x)]
  (merge task (if (number? pos)
                  {:ToExchange {:Servo_PPC_Pos {:Value pos :Unit "step"}}}
                  {:Retry true
                   :ToExchange {:Servo_PPC_Pos false }}))))
