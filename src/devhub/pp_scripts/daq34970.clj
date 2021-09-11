(ns devhub.pp-scripts.daq34970
  ^{:author "Wact B. Prot <wactbprot@gmail.com>"
    :doc "PostProcessing for a DAQ34970."}
  (:require [clojure.string  :as string]
            [devhub.pp-utils :as ppu]
            [devhub.utils :as u]))

(defn split-line [s] (string/split s #","))

(defn count-ok? [v n] (= (count v) n))

(defn temperature-scanner-read-out [{{chan :Channels prefix :Prefix suffix :Suffix} :PostScriptInput x :_x :as task}]
  (let [n (count chan)
        v (mapv  #(mapv u/number (split-line %)) x)
        v (ppu/transpose (filterv #(count-ok? % n) v))]
    (merge task {:Result (mapv (fn [ch v] (ppu/vl-result (str prefix ch suffix) v "C")) chan v)})))
