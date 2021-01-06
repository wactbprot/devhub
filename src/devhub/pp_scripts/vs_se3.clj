(ns devhub.pp-scripts.vs_se3
  (:require [devhub.pp-scripts.utils :as ppu]
            [devhub.utils            :as u]))

(def conf (u/config "vs_se3.edn"))

(defn registers-ok?
  "Checks `rs` for type vector and checks the length to
  be `(:register-count conf)`."
  [rs]
  (and (vector? rs) (=  (count rs) (:register-count conf))))

(defn check
  "Returns a vector of maps.
  
  Example:
  ```clojure
  (check [89] {:ThingA [0 0] :ThingB [0 1]})
  ;; =>
  ;; [{:ThingA true} {:ThingB false}]
  ```
  "
  [rs m]
  (mapv
   (fn [[kw [block position]]] {kw (ppu/open? (nth rs block) position)})
   m))

(defn valves
  "Returns exchange structures like

  ```json
  {ToExchange:
    {E7_open:    {Bool: true},
     E3_open:    {Bool: true},
     E15_open:   {Bool: true},
     E10_open:   {Bool: false},
     E20_open:   {Bool: false},
     E18_open:   {Bool: true},
     E10_closed: {Bool: true},
     ...
     registers [89, 0, -102, 0, -102, 0, 21, 0, -91]}}
  ```
  
  Example:
  ```clojure
  (valves {} {:_x [85 0 16 0 16 0 21 0 7]})
  ```"
  [task {rs :_x}]
  (if (registers-ok? rs)
    (let [vs (ppu/exch-bool-map (check rs (:valve-position conf)))]
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
  (switches {} {:_x [89 0 -102 0 -102 0 21 0 -91]})
  ```"
  [task {rs :_x}]
  (if (registers-ok? rs)
    (let [so (ppu/exch-bool-map (check rs (:switch-open    conf)))
          sc (ppu/exch-bool-map (check rs (:switch-closed  conf)))]
      {:ToExchange (reduce merge (reduce merge {:registers rs} so) sc)})
    {:error "wrong register format"}))

