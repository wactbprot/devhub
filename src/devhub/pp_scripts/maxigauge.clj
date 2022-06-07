(ns devhub.pp-scripts.maxigauge
  ^{:author "Thomas Bock <wactbprot@gmail.com>"
    :doc "Post processing for Maxigauge controller."}
  (:require [clojure.string :as string]
            [devhub.pp-utils :as ppu]
            [devhub.utils :as u]))

(def test-vec [""
               "0,-2.4342E+00"
               ""
               "0,+9.4771E-05"
               ""
               "0,+1.4055E-03"
               ""
               "0,-1.4192E-04"
               ""
               "0,+4.5151E-04"])
(def test-imnput {:CH1 {:Reservoir 3, :Fullscale 1000, :Unit "mbar"},
 :CH2 {:Reservoir 3, :Fullscale 10, :Unit "mbar"},
 :CH3 {:Reservoir 4, :Fullscale 10, :Unit "mbar"},
 :CH4 {:Reservoir 4, :Fullscale 0.1, :Unit "mbar"},
 :CH5 {:Reservoir 5, :Fullscale 10, :Unit "mbar"},
 :CH6 {:Reservoir 5, :Fullscale 0.1, :Unit "mbar"}})

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
