(ns devhub.pp-scripts.vs-se3
  (:require [devhub.config :as c]
            [devhub.pp-utils :as ppu]))

(def conf (c/get-conf "vs_se3.edn"))

(defn registers-ok?
  "Checks `rs` for type vector and checks the length to
  be `(:register-count conf)`."
  [rs]
  (and (vector? rs) (= (count rs) (:register-count conf))))

(defn check
  "Returns a vector of maps. The register `b`lock and
  `p`osition is provided by the `conf`iguration.
  
  Example:
  ```clojure
  (check [89] {:ThingA [0 0] :ThingB [0 1]})
  ;; =>
  ;; [{:ThingA true} {:ThingB false}]
  ```
  "
  [rs m]
  (mapv (fn [[kw [b p]]] {kw (ppu/open? (nth rs b) p)}) m))

(defn set-valve
  "The PreScript vs_se3.set-valve calculates the new register value
  depending on the current state. The current state is provided by the
  registers PreInput. The `:Address`, register (`b`lock) and
  `p`osition is provided by the `conf`iguration.
 
  Example:
  ```clojure
  (set-valve {:PreInput {
                 :registers [1029 0 4100 0 1300 0 21248 0 83]
                 :valve \"V1\"
                 :should \"open\" }})
  ```"
  [task]
  (let [input  (:PreInput task)
        rs              (:registers input)
        valve  (keyword (:valve     input))
        should (keyword (:should    input))]
    (if (and (registers-ok? rs) valve should)
      (let [[b p] (valve (:valve-position conf))
            a     (valve (:valve-set-addr conf))
            r     (ppu/modify-register (nth rs b) p should)]
        (assoc task :Value [r] :Address a))
      (merge task {:error "missing or wrong :registers, :valve or :should"}))))

(defn valves
  "Returns the human readable state of the valves derived from the `reg`ister`s`.

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
  (get-valves {} {:_x [85 0 16 0 16 0 21 0 7]})
  ```"
  [task]
  (let [rs (:_x task)]
    (if (registers-ok? rs)
      (let [vs (ppu/exch-bool-map (check rs (:valve-position conf)))]
        (merge task {:ToExchange (reduce merge {:registers rs} vs)}))
      (merge task {:error "wrong register format"}))))
  
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
  [task]
  (let [rs (:_x task)]
    (if (registers-ok? rs)
      (let [so (ppu/exch-bool-map (check rs (:switch-open    conf)))
            sc (ppu/exch-bool-map (check rs (:switch-closed  conf)))]
        (merge task {:ToExchange (reduce merge (reduce merge {:registers rs} so) sc)}))
      (merge task {:error "wrong register format"}))))

