(ns devhub.pp-scripts.mks-srg3
  ^{:author "Thomas Bock <wactbprot@gmail.com>"
    :doc "Post processing for MKS SRG3  controller."}
  (:require [clojure.string :as string]
            [devhub.pp-utils :as ppu]
            [devhub.utils :as u]))


(defn extract [s]
  (when (string? s)
    (let [r #">{1,3}\s([0-9]\.[0-9]{4}[E][-+][0-9]{2})\s$"]
      (second (re-matches r s)))))

(defn read-out [{{unit :Unit token :Type} :PostScriptInput x :_x :as task}]
  (let [v (mapv extract x)
        o (ppu/operable v)
        y (ppu/calc-seq v o)]
    (assoc task :Result [(ppu/vl-result token (ppu/mean y) unit)])))


(defn read-with-slope [{{unit :Unit token :Type} :PostScriptInput x :_x t0 :_t_start t1 :_t_stop :as task}]
  (let [v (mapv extract x)
        o (ppu/operable v)
        y (ppu/calc-seq v o)
        t (ppu/t0t1->t (ppu/calc-seq t0 o) (ppu/calc-seq t1  o))]
    (assoc task
           :Result [(ppu/vl-result token (ppu/mean y) unit)
                    (ppu/vl-result token  (ppu/slope y t) (str unit "/ms"))
                    (ppu/vl-result (str token "_R") (ppu/r-square y t) "1")])))

(defn read-freq [{{unit :Unit token :Type} :PostScriptInput x :_x :as task}]
  (let [v (mapv extract x)
        o (ppu/operable v)
        y (ppu/calc-seq v o)]
    (prn [(ppu/vl-result token (ppu/mean y) unit)])
    (assoc task :Result [(ppu/vl-result token (ppu/mean y) unit)])))
