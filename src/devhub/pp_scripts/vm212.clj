(ns devhub.pp-scripts.vm212
  ^{:author "Thomas Bock <wactbprot@gmail.com>"
    :doc "Post processing for SRG controller vm212."}
  (:require [devhub.pp-utils :as ppu]
            [devhub.utils :as u]))

(defn dcr-extract [s]
  (let [r #"\s*DCR\s*([-+][0-9]{1}\.[0-9]{3,5}[E][-+][0-9]{2})\r\n"]
    (second (re-matches r s))))

(defn dcr-read-out
  "First Value is droped by `(rest x)` because often there is an old
  value stored in the device buffer."
  [{x :_x input :PostScriptInput :as task}]
  (let [v (mapv dcr-extract (rest x))
        o (ppu/operable v)]
    (merge task {:Result [(ppu/vl-result (:Type input) (ppu/calc-seq v o) "DCR")]})))
