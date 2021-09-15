(ns devhub.pp-scripts.daq34970
  ^{:author "Thomas Bock <wactbprot@gmail.com>"
    :doc "daq34970 post processing."}
  (:require [clojure.string :as string]
            [devhub.pp-utils :as ppu]
            [devhub.utils :as u]))

(defn split-line [s] (string/split s #","))

(defn count-ok? [v n] (= (count v) n))

(defn temperature-scanner-read-out [{{chan :Channels prefix :Prefix suffix :Suffix} :PostScriptInput :as task}]
  (let [n (count chan)
        v (mapv  (fn [l] (mapv u/number (split-line l))) (:_x task))
        v (ppu/transpose (filterv (fn [v] (count-ok? v n)) v))]
    (merge task {:Result (mapv (fn [ch v] (ppu/vl-result (str prefix ch suffix) v "C")) chan v)})))
