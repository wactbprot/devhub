(ns devhub.pp-scripts.maxigauge
  (:require [clojure.string  :as string]
            [devhub.pp-utils :as ppu]
            [devhub.utils    :as u]))

(defn read-out [task]
  (let [input (:PostScriptInput task)
        v     (mapv (fn [s] (u/number (second (string/split s #","))))(:_x task))]
    (merge task {:Result (ppu/vl-result (or (:Type input) "ind") v (or (:Unit input) "mbar"))})))
