(ns devhub.pp
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
  (let [ps (keyword (:PostScript task))]
    (condp = ps
      :vs_se3.valves         (vs-se3/valves         task data)
      :vs_se3.switches       (vs-se3/switches       task data)
      :gn_se3.anybus-readout (gn-se3/anybus-readout task data)
      {:error (str "no :PostScript named: " ps)})))


(defn pre-dispatch
  [conf task]
  (let [ps (keyword (:PreScript task))]
    (condp = ps
      :vs_se3.set-valve (vs-se3/set-valve task)
      {:error (str "no :PreScript named: " ps)})))
    
