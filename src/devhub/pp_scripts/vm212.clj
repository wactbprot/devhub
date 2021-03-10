(ns devhub.pp-scripts.vm212
  (:require [devhub.pp-utils :as ppu]
            [devhub.utils    :as u]))

(defn dcr-extract
  [s]
  (let [r #"\s*DCR\s*([-+][0-9]{1}\.[0-9]{3,5}[E][-+][0-9]{2})\r\n"]
        (second (re-matches r s))))

(defn dcr-read-out
  "First Value is droped because often ther is an old value stored in the
  device buffer."
  [task]
  (let [input (:PostScriptInput task)
        v     (mapv dcr-extract (rest (:_x task)))
        o     (ppu/operable v)]
    (merge task {:Result [(ppu/vl-result (:Type input) (ppu/calc-seq v o) "DCR")]})))
