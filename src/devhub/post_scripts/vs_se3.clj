(ns devhub.post-scripts.vs_se3
  (:require [devhub.post-scripts.utils :as u]))

(def conf (u/config "resources/vs_se3.edn"))

(defn registers-ok?
  "Checks `rs` for type vector and checks the length to be `(:register-count conf)`."
  [rs] (and (vector? rs) (=  (count rs) (:register-count conf))))

(defn check
  "Returns a vector of maps.
  
  Example:
  ```clojure
  (check [1025] {:ThingA [0 0] :ThingB [0 1]})
  ;; =>
  ;; [{:ThingA true} {:ThingB false}]
  ```
  "
  [rs m]
  (mapv
   (fn [[kw [block position]]] {kw (u/open? (nth rs block) position)})
   m))

(defn valves
  "Returns exchange structures like

  ```json
  ToExchange:{V1:{ Bool: true}, V2:{ Bool:false}
  ```
  
  Example:
  ```clojure
  (valves {} {:_x [1025, 0, 21760, 0, 0, 0, 1024, 0, 7]})
  ```"
  [task {rs :_x}]
  (if (registers-ok? rs)
    (let [vs (u/exch-bool-map (check rs (:valve-position conf)))]
      {:ToExchange (reduce merge {:registers rs} vs)})
    {:error "wrong register format"}))

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
  [task {rs :_x}]
  (if (registers-ok? rs)
    (let [so (u/exch-bool-map (check rs (:switch-open    conf)))
          sc (u/exch-bool-map (check rs (:switch-closed  conf)))]
      {:ToExchange (reduce merge (reduce merge {:registers rs} so) sc)})
    {:error "wrong register format"}))

