(ns devhub.pp-scripts.maxigauge
  ^{:author "Thomas Bock <wactbprot@gmail.com>"
    :doc "Post processing for Maxigauge controller."}
  (:require [clojure.string :as string]
            [devhub.pp-utils :as ppu]
            [devhub.utils :as u]))

(defn rm-ack [v] (filterv #(< 1 (count %)) v))

(defn extract-value
  "Extracts the value from the given string if the string starts with
  a `0`. Returns `nil` otherwise."
  [s]
  (let [v (string/split s #",")]
    (when (and (= (count v) 2)
               (= (first v) "0"))
      (-> v second u/number))))

(defn read-out [{{t :Type u :Unit} :PostScriptInput x :_x :as task}]
  (let [v (mapv extract-value x)
        r (ppu/vl-result t v u)]
    (merge task {:Result r})))


(defn read-all [{input :PostScriptInput x :_x :as task}]
  (prn x)
  task

  )
