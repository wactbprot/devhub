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
  [task]
  (let [i (:PostScriptInput task)
        v  (mapv lb-extract (rest (:_x task)))
        o  (ppu/operable v)]
    (merge task {:Result [(ppu/vl-result (:Type i) (ppu/calc-seq v o) "lb")]})))
