(ns devhub.pp-scripts.utils
  (:require [clojure.edn        :as edn]
            [devhub.utils       :as u]
            [jdk.nio.ByteBuffer :as bb]))

;;------------------------------------------------------------
;; ToExchange tools
;;------------------------------------------------------------
(defn first-key [m] (when (map? m) (first (keys m))))
(defn first-val [m] (when (map? m) (first (vals m))))

(defn bool->exch-map [b] (if b {:Bool true} {:Bool false})) 

(defn exch-bool-map
  [v]
  (mapv (fn [m] {(first-key m) (bool->exch-map (first-val m))}) v)) 

;;------------------------------------------------------------
;; bits & bytes
;;------------------------------------------------------------
(defn open? [r n] (bit-test r n))

(defn b16->h [b] (bit-and (bit-shift-right b 8) 0xff))

(defn b16->l [b] (bit-and  b 0xff))

(defn vec->float [v] (-> v byte-array bb/*wrap bb/get-float))

;;------------------------------------------------------------
;; ensure operable
;;------------------------------------------------------------
(defn operable
  "Returns a vector of booleans indicating if the values are
  usable in calculations.

  Example:
  ```clojure
  (operable [\"1\" 1.234E-5 0 \"a\" :number])
  ;; =>
  ;; [true true true false false]
  (operable 1)
  ;; =>
  ;; nil
  ```"
  [v]
  (when (seqable? v) (mapv (comp number? u/number) v)))

(defn operable-seq
  "Shrinks the vector `v` down to operable values depending on vector `o`.

  Example:
  ```clojure
  (operable-seq [\"1\" 1.234E-5 0    \"a\"  :number]
                [true  true     true false  false])  
  ;; =>
  ;; [\"1\" 1.234E-5 0]
  ```"
  [v o]
  (when (and (seqable? v) (seqable? o))
    (mapv :value
          (filter (fn [{ok? :take}] (when ok? :take))
                  (mapv (fn [x y] {:take x :value y}) o v)))))
  
(defn calc-seq
  "Same as [[operable-seq]] but returns a vector of numbers.

  Example:
  ```clojure
    (calc-seq [\"1\" 1.234E-5 0    \"a\"  :number]
                [true  true     true false  false])  
  ;; =>
  ;; [1 1.234E-5 0]
  ```"

  [v o]
  (when (and (seqable? v) (seqable? o))
    (mapv u/number (operable-seq v o))))