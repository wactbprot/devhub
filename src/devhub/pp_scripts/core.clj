(ns devhub.pp-scripts.core
  (:require [devhub.pp-scripts.vs_se3 :as vs-se3]))

(defn dispatch
  [conf task data]
  (condp = (keyword (:PostScript task))
    :vs_se3.valves   (vs-se3/valves   task data)
    :vs_se3.switches (vs-se3/switches task data)
    data))
        