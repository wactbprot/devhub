(ns devhub.pp
  (:require [devhub.pp-scripts.gn_se3 :as gn-se3]
            [devhub.pp-scripts.vs_se3 :as vs-se3]
            [devhub.pp-scripts.im540  :as im540]
            ))

(defn post-dispatch
  "TODO: make auto dispatch
  
  Example:
  ```clojure
  ;; e.g.:
  (keys (ns-publics 'devhub.pp-scripts.vs_se3))
  ;; =>
  ;; (conf registers-ok? check valves switches)
  ```"
  [conf task]
  (let [ps (keyword (:PostScript task))]
    (condp = ps
      :vs_se3.valves          (vs-se3/valves          task)
      :vs_se3.switches        (vs-se3/switches        task)
      :gn_se3.anybus-readout  (gn-se3/anybus-readout  task)
      :gn_se3.anybus-add-ctrl (gn-se3/anybus-add-ctrl task)
      :gn_se3.anybus-add-read (gn-se3/anybus-add-read task)
      :gn_se3.anybus-add-loss (gn-se3/anybus-add-loss task)
      :im540.read-out         (im540/read-out         task)
      :im540.pressure-rise    (im540/pressure-rise    task)
      {:error (str "no :PostScript named: " ps)})))


(defn pre-dispatch
  [conf task]
  (let [ps (keyword (:PreScript task))]
    (condp = ps
      :vs_se3.set-valve (vs-se3/set-valve task)
      {:error (str "no :PreScript named: " ps)})))
    
