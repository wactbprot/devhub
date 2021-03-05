(ns devhub.pp-scripts.daq34970
  (:require [clojure.string  :as string]
            [devhub.pp-utils :as ppu]
            [devhub.utils    :as u]))

(defn split-line [s] (string/split s #","))

(defn count-ok? [v n] (= (count v) n))

(defn temperature-scanner-read-out
  [task]
  (let [input (:PostScriptInput task)
        chan  (:Channels input) prefix (:Prefix input) suffix (:Suffix input)
        n     (count chan)
        v     (mapv  (fn [l] (mapv u/number (split-line l))) (:_x task))
        v     (filterv (fn [v] (count-ok? v n)) v)
        ;; matrix transpose lol:
        ;; https://clojuredocs.org/clojure.core/apply#example-542692cdc026201cdc326d4d
        v     (apply mapv vector v)]
    (merge task {:Result (mapv (fn [ch v] (ppu/vl-result (str prefix ch suffix) v "C")) chan v)})))
