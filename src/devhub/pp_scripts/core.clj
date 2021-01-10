(ns devhub.pp-scripts.core
  (:require [devhub.pp-scripts.vs_se3 :as vs-se3]
            [devhub.pp-scripts.gn_se3 :as gn-se3]))

(defn post-dispatch
  "TODO: make auto dispatch
  
  Example:
  ```clojure
  ;; e.g.:
  (keys (ns-publics 'devhub.pp-scripts.vs_se3))
  ;; =>
  ;; (conf registers-ok? check valves switches)
  ```"
  [conf task data]
  (condp = (keyword (:PostScript task))
    :vs_se3.valves   (vs-se3/valves               task data)
    :vs_se3.switches (vs-se3/switches             task data)
    :gn_se3.anybus-readout (gn-se3/anybus-readout task data)
    data))


(defn pre-dispatch [conf task] task)
