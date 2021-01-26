(ns devhub.pp-scripts.gn_se3
  (:require [devhub.pp-utils :as ppu]
            [devhub.utils    :as u]
            [com.brunobonacci.mulog :as µ]))

(def conf (u/config "gn_se3.edn"))

(defn anybus-vec
  "Returns a vector of 4 bytes that can be turned into a float.
  ```clojure
  (anybus-vec 32832 99 46336 257)
  ;; =>
  ;; [64 0 99 181]
  ```"
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
