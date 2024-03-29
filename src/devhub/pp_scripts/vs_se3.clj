(ns devhub.pp-scripts.vs-se3
  ^{:author "Thomas Bock <wactbprot@gmail.com>"
    :doc "Pre/Post processing for SE3 valve controller."}
  (:require [devhub.config :as c]
            [devhub.pp-utils :as ppu]))

(def conf {
 ;;; valve position [block-position bit-position]
           :valve-position  {:V1  [0 0] :V2  [0 2] :V3  [0 4] :V4  [0 6]
                             :V5  [2 0] :V6  [2 2] :V7  [2 4] :V8  [2 6]
                             :V9  [4 4] :V10 [4 2] :V11 [4 0] :V12 [4 6]
                             :V13 [6 0] :V14 [6 2] :V15 [6 4] :V16 [6 6]
                             :V17 [8 0] :V18 [8 1] :V19 [8 2] :V20 [8 3]}
           :valve-set-addr  {:V1  40003 :V2  40003 :V3  40003 :V4  40003
                             :V5  40004 :V6  40004 :V7  40004 :V8  40004 
                             :V9  40005 :V10 40005 :V11 40005 :V12 40005
                             :V13 40006 :V14 40006 :V15 40006 :V16 40006
                             :V17 40007 :V18 40007 :V19 40007 :V20 40007}
           :switch-open     {:E1_open  [0 0] :E2_open  [0 2] :E3_open  [0 4] :E4_open  [0 6]
                             :E5_open  [2 0] :E6_open  [2 2] :E7_open  [2 4] :E8_open  [2 6]
                             :E9_open  [4 4] :E10_open [4 2] :E11_open [4 0] :E12_open [4 6]
                             :E13_open [6 0] :E14_open [6 2] :E15_open [6 4] :E16_open [6 6]
                             :E17_open [8 0] :E18_open [8 2] :E19_open [8 4] :E20_open [8 6]}
           :switch-closed   {:E1_closed  [0 1] :E2_closed  [0 3] :E3_closed  [0 5] :E4_closed  [0 7]
                             :E5_closed  [2 1] :E6_closed  [2 3] :E7_closed  [2 5] :E8_closed  [2 7]
                             :E9_closed  [4 5] :E10_closed [4 3] :E11_closed [4 1] :E12_closed [4 7]
                             :E13_closed [6 1] :E14_closed [6 3] :E15_closed [6 5] :E16_closed [6 7]
                             :E17_closed [8 1] :E18_closed [8 3] :E19_closed [8 5] :E20_closed [8 7]}
 ;;; quantity is the number of registers to get all valves at once
           :register-count 9})

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
  [{input :PreInput :as task}]
  (let [rs              (:registers input)
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
  (get-valves {:_x [85 0 16 0 16 0 21 0 7]})
  ```"
  [{rs :_x :as task}]
  (if (registers-ok? rs)
    (let [vs (ppu/exch-bool-map (check rs (:valve-position conf)))]
      (merge task {:ToExchange (reduce merge {:registers rs} vs)}))
    (merge task {:error "wrong register format"})))

(defn switches
  "Returns exchange structures like
  
  ```json
  ToExchange:
  {E1_open:{ Bool:true}, E1_closed:{Bool:false}
  ```
  
  Example:
  ```clojure
  (switches {:_x [89 0 -102 0 -102 0 21 0 -91]})
  ```"
  [{rs :_x :as task}]
  (if (registers-ok? rs)
    (let [so (ppu/exch-bool-map (check rs (:switch-open    conf)))
          sc (ppu/exch-bool-map (check rs (:switch-closed  conf)))]
      (merge task {:ToExchange (reduce merge (reduce merge {:registers rs} so) sc)}))
    (merge task {:error "wrong register format"})))
