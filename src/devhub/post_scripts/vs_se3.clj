(ns devhub.post-scripts.vs_se3
  (:require [clojure.edn  :as edn]
            [devhub.utils :as u]
            [devhub.conf  :as c]))

(defn config [] (-> "resources/vs_se3.edn" slurp edn/read-string))

(defn open? [x n] (bit-test x n))

(defn input-ok?
  [x]
  (and (vector? x)
       (= (:all-block-count (config))
          (count x))))

(defn check
  [x m]
  (when (input-ok? x)
    (mapv
     (fn [[kw [block position]]] {kw (open? (nth x block) position)})
     (seq m))))

(defn valves-state    [x] (check x (:valve-position (config))))
(defn switches-open   [x] (check x (:switch-open    (config))))
(defn switches-closed [x] (check x (:switch-closed  (config))))

(defn first-key [m] (when (map? m) (first (keys m))))
(defn first-val [m] (when (map? m) (first (vals m))))

(defn bool->exch-map [b] (if b {:Bool 1} {:Bool 0})) 

(defn exch-valves
  [vs]
  (mapv (fn [m]
          {(first-key m)
           (bool->exch-map (first-val m))})
        vs)) 

(defn valves
  "Returns exchange structures like

  ```json
  ToExchange:{V1:{ Bool:1}, V2:{ Bool:0}
  ```
  
  Example:
  ```clojure
  (valves {} {:_x [1025, 0, 21760, 0, 0, 0, 1024, 0, 7]})
  ```"
  [input {data :_x}]
  (if-let [vs (valves-state data)]
    {:ToExchange (reduce merge {:valve-register data} (exch-valves vs))}
    {:error "wrong data"}))


(defn switches
  "Returns exchange structures like
  
  ```json
  ToExchange:
  {E1_open:{ Bool:true}, E1_closed:{Bool:false}
  ```
  
  Example:
  ```clojure
  (switches {} {:_x [1025, 0, 21760, 0, 0, 0, 1024, 0, 7]})
  ```"
    [input data]
  {})
