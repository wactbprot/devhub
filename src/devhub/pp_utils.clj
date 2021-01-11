(ns devhub.pp-utils
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

(def square (fn [x] (* x x)))

(defn mean [v] (when (pos? (count v)) (/ (reduce + v) (count v))))

(defn stdev
  "Calculates the standard deviation of the vector `v`.

  Example:
  ```clojure
  (stdev [0 1 2])
  ;; =>
  ;; 1.0

  (stdev [0])
  ;; =>
  ;; nil

  (stdev [])
  ;; =>
  ;; nil

  (stdev nil)
  ;; =>
  ;; nil

  ```"
  [v]
  (let [ndec (dec (count v)) mv (mean v)]
    (when (and mv (pos? ndec))
      (Math/sqrt (/ 
                  (reduce (fn [a b] (+ a (square (- b  mv)))) 0 v)
                  ndec)))))


(defn vl-result
  "Returns the vl result structure at least consisting of `:Type` `t`
  `:Value` `v` and `:Unit` `u`.

  Example:
  ```clojure
  (vl-result \"ind\" [1] \"Pa\")
  ;; =>
  ;; {:Type \"ind\", :Value 1, :Unit \"Pa\"}

  (vl-result \"ind\" 1 \"Pa\")
  ;; =>
  ;; {:Type \"ind\", :Value 1, :Unit \"Pa\"}

  (vl-result \"ind\" [0 1 2] \"Pa\")
  ;; =>
  ;; {:Type \"ind\", :Value 1, :Unit \"Pa\", :SdValue 1.0, :N 3}
  ```"
  [t v u]
  (if (coll? v)
    (let [m {:Type t :Value (mean v) :Unit u}]
      (if-let [sd (stdev v)]
        (assoc m :SdValue sd :N (count v))
        m))
    {:Type t :Value v :Unit u}))
