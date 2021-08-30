(ns devhub.pp-scripts.frs5
  (:require [devhub.pp-utils :as ppu]
            [devhub.utils    :as u]))


(def s "N     + 0.000002 lb ")


(defn lb-extract
  [s]
  (let [r #"^N\s*([-+])\s*([0-9]*\.[0-9]*)\s*lb\s*"
        v (re-matches r s)]
    (when (= (count v) 3)
      (str (nth v 1) (nth v 2)))))

(defn lb-read-out
  "Calculates the result after removing the first 5 values."
  [task]
  (let [i (:PostScriptInput task)
        v  (mapv lb-extract (nthrest (:_x task) 5))
        o  (ppu/operable v)]
    (merge task {:Result [(ppu/vl-result (:Type i) (ppu/calc-seq v o) "lb")]})))
