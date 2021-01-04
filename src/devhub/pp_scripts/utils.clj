(ns devhub.pp-scripts.utils
  (:require [clojure.edn  :as edn]))

(defn open? [r n] (bit-test r n))

(defn first-key [m] (when (map? m) (first (keys m))))
(defn first-val [m] (when (map? m) (first (vals m))))

(defn bool->exch-map [b] (if b {:Bool true} {:Bool false})) 

(defn exch-bool-map
  [vs]
  (mapv
   (fn [m] {(first-key m) (bool->exch-map (first-val m))})
   vs)) 


