(ns devhub.pp-scripts.gn-se3
  (:require [devhub.pp-utils :as ppu]
            [devhub.utils    :as u]
            [com.brunobonacci.mulog :as µ]))

(def conf
  ;; device name start byte
  {:anybus-byte-start {"1T_1" 0 "1T_2" 4 "1T_3" 8
                       "10T_1" 12 "10T_2" 16 "10T_3" 20
                       "100T_1" 24 "100T_2" 28 "100T_3" 32
                       "1000T_1" 36 "1000T_2" 40 "1000T_3" 44
                       "5T_1" 48 "50T_1" 52 "500T_1" 56
                       "add" 60}
   :anybus-byte-count 4})

(defn anybus-vec
  "Returns a vector of 4 bytes that can be turned into a float."
  [v]
  [(ppu/b16->l (nth v 0))
   (ppu/b16->h (nth v 1))
   (ppu/b16->l (nth v 1))
   (ppu/b16->h (nth v 2))])


(defn anybus-float
  "Converts `byte-array` to `float`.
  
  Example:
  ```clojure
  ;; approx. 2mbar:
  (def  v [32825 23151 30976 257 32826 17559 34304 257 32826 6375 41472 257 ; 1T 
          32832 161 9216 257 32832 120 12032 257 32832 161 9216 257
          32832 338 40448 257 32832 202 6400 257 32832 338 40448 257
          32831 65530 41216 257 32831 61434 63232 257 32831 65530 41216 257
          32832 150 59136 257 32832 31 29184 257 32831 63482 52224 257 ; 5T..
          32832 99 46336 257])
  (anybus-float v 12 4)
  ;; =>
  ;; 2.0098352
  (anybus-float v 12 4 \"kPa\")
  ;; =>
  ;; 0.20098352432250977  
  ```"
  ([v s n]
   (ppu/vec->float (anybus-vec (subvec v s (+ s n)))))
  ([v s n u]
   (let [x (anybus-float v s n)]
     (condp = (keyword (or u "mbar"))
       :mbar  x
       :Pa    (* x 100)
       :kPa   (/ x 10)
       (µ/log ::anybus-float :error "conversion not implemented")))))

(defn anybus-extract
  [task start n u]
  (if (u/single-meas? task)
    (anybus-float (:_x task) start n u)
    (mapv (fn [v] (anybus-float v start n u)) (:_x task))))

(defn anybus-readout
  "Returns `Result` and `ToExchange` maps.
  
  NOTE: The anybus gateway is configured to deliver pressures in `mbar`."
  [task]
  (let [input (:PostScriptInput task)
        pre   (:Prefix input) suf (:Suffix input) unit (:Unit input)
        m     (:anybus-byte-start conf) n (:anybus-byte-count conf)
        f     (fn [[dev-name start]]
                (ppu/vl-result (str pre dev-name suf)
                               (anybus-extract task start n unit)
                               unit)) 
        res (mapv f m)]
    (merge task
           {:Result res
            :ToExchange (into {} (map (fn [m] {(:Type m) m}) res))})))

(defn ok?
  [p_current p_target]
  (and (number? p_target)
       (number? p_current) 
       (> p_current  (* p_target  (- 1 0.02)))
       (< p_current  (* p_target  (+ 1 0.02)))))

(defn anybus-add-ctrl
  [task]
  (let [m         (:anybus-byte-start conf) n (:anybus-byte-count conf)
        input     (:PostScriptInput task)
        p_target  (u/number (:TargetPressure input)) unit (:TargetUnit input)
        p_current (ppu/mean (anybus-extract task (get m "add") n unit))]
    (merge task
           {:ToExchange {:Pressure_fill_check {:Value p_current
                                               :Unit unit
                                               :Type "fill_check"}
                         :Pressure_fill_ok {:Bool (ok? p_current p_target)}}})))

(defn anybus-add-read
  [task]
  (let [m     (:anybus-byte-start conf) n (:anybus-byte-count conf)
        input (:PostScriptInput task)
        token (:Type input) unit (:Unit input)
        v     (anybus-extract task (get m "add") n unit)]
    (merge task {:Result  [(ppu/vl-result token v unit)]})))

(defn anybus-add-loss
  [task]
  (let [m     (:anybus-byte-start conf) n (:anybus-byte-count conf)
        input (:PostScriptInput task)
        token (:Type input) unit (:Unit input)
        v     (anybus-extract task (get m "add") n unit)
        o     (ppu/operable v)
        y     (ppu/calc-seq v o)
        t     (ppu/t0t1->t (ppu/calc-seq (:_t_start task) o)
                           (ppu/calc-seq (:_t_stop task)  o))]
    (merge task {:Result [(ppu/vl-result (str token "_slope_x")
                                         (ppu/slope y t)
                                         "mbar/ms")]})))

(defn anybus-pressure-ctrl
  [task]
  (let [m         (:anybus-byte-start conf) n (:anybus-byte-count conf)
        input     (:PostScriptInput task)
        p_target  (u/number (:TargetPressure input)) unit (:TargetUnit input)
        p_current (cond
                    (<= p_target 133)    (ppu/mean (concat
                                                   (anybus-extract task (get m "1T_1") n unit)
                                                   (anybus-extract task (get m "1T_2") n unit)
                                                   (anybus-extract task (get m "1T_3") n unit)))
                    (<= p_target 1333)   (ppu/mean (concat
                                                   (anybus-extract task (get m "10T_1") n unit)
                                                   (anybus-extract task (get m "10T_2") n unit)
                                                   (anybus-extract task (get m "10T_3") n unit)))
                    (<= p_target 13332)  (ppu/mean (concat
                                                   (anybus-extract task (get m "100T_1") n unit)
                                                   (anybus-extract task (get m "100T_2") n unit)
                                                   (anybus-extract task (get m "100T_3") n unit)))
                    (<= p_target 133322) (ppu/mean (concat
                                                   (anybus-extract task (get m "1000T_1") n unit)
                                                   (anybus-extract task (get m "1000T_2") n unit)
                                                   (anybus-extract task (get m "1000T_3") n unit))))]
    (if-not (= unit "Pa")
      (merge task {:error "target unit not implemented"}) 
      (merge task
             {:ToExchange {:Pressure_fill_check {:Value p_current
                                                 :Unit unit
                                                 :Type (:Type input)}
                           :Pressure_fill_ok {:Bool (ok? p_current p_target)}
                           :Pressure_compare_check {:Value p_current
                                                 :Unit unit
                                                 :Type (:Type input)}
                           :Pressure_compare_ok {:Bool (ok? p_current p_target)}}}))))
