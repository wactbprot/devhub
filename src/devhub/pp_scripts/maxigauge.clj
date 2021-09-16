(ns devhub.pp-scripts.maxigauge
  ^{:author "Thomas Bock <wactbprot@gmail.com>"
    :doc "Post processing for Maxigauge controller."}
  (:require [clojure.string :as string]
            [devhub.pp-utils :as ppu]
            [devhub.utils :as u]))

(defn read-out [{input :PostScriptInput x :_x :as task}]
  (let [v (mapv (fn [s] (u/number (second (string/split s #",")))) x)]
    (merge task {:Result (ppu/vl-result (or (:Type input) "ind") v (or (:Unit input) "mbar"))})))
