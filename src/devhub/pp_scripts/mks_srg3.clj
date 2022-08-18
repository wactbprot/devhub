(ns devhub.pp-scripts.mks-srg3
  ^{:author "Thomas Bock <wactbprot@gmail.com>"
    :doc "Post processing for MKS SRG3  controller."}
  (:require [devhub.pp-utils :as ppu]
            [devhub.utils :as u]))


(defn extract [s]
  (let [r #">\s([0-9]\.[0-9]{4}[E][-+][0-9]{2})\s$"]
  (second (re-matches r s))))


(defn read-out [{{unit :Unit} :PostScriptInput x :_x t0 :_t_start t1 :_t_stop :as task}]
  (let [v (mapv extract x)
        o (ppu/operable v)
        y (ppu/calc-seq v o)
        t (ppu/t0t1->t (ppu/calc-seq t0 o) (ppu/calc-seq t1  o))]
    (prn o)
    (prn t0)
    (prn t1)))
