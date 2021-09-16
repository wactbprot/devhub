(ns devhub.pp-scripts.inf-cube
  ^{:author "Thomas Bock <wactbprot@gmail.com>"
    :doc "Post processing for Inficon cube controller."}
  (:require [devhub.pp-utils :as ppu]
            [devhub.utils    :as u]))

(def test-vec ["PRE","2.98501","","SL>PRE","2.957324","",
               "SL>PRE","2.957747","","SL>PRE"," -0.00445",""])

(defn extract [s]
  (let [r #"[-]*[0-9]*\.[0-9][E]*[-+]*[0-9]*"]
    (re-matches r s)))

(defn val-vec [v]
  (let [v  (mapv extract v)
        o  (ppu/operable v)]
    (ppu/calc-seq v o)))

(defn readout [task]
  (merge task {:Result [(ppu/vl-result (get-in task [:PostScriptInput :Type])
                                       (val-vec (:_x task))
                                       (get-in task [:PostScriptInput :Unit]))] }))

(defn readout-vec [task]
  (merge task {:Result [{:Type (get-in task [:PostScriptInput :Type])
                         :Value (val-vec (:_x task))
                         :Unit (get-in task [:PostScriptInput :Unit])}] }))
