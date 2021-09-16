(ns devhub.pp-scripts.mks627-kunbus
  ^{:author "Thomas Bock <wactbprot@gmail.com>"
    :doc "Post processing for MKS CDGs Type 627 read out over Kunbus- 
          Modbus/Profibus gateway."}
  (:require [devhub.pp-utils :as ppu]
            [devhub.utils :as u]))

(defn kunbus-vec
  "Returns a vector of  bytes that can be turned into a float."
  [v]
  [(ppu/b16->h (nth v 3))
   (ppu/b16->l (nth v 4))
   (ppu/b16->h (nth v 4))
   (ppu/b16->l (nth v 5))])

(defn val-vec [{x :_x}] (mapv #(ppu/vec->float (kunbus-vec %)) x))

(defn readout-first
  "Returns `Result` and `ToExchange` maps.
  
  NOTE: The kunbus gateway is configured to deliver pressures in `mbar`."
  [task]
  (merge task {:Result [(ppu/vl-result (get-in task [:PostScriptInput :Type])
                                      (val-vec task)
                                      "mbar")] }))

(defn readout-first-vec [task]
  (merge task {:Result [{:Type (get-in task [:PostScriptInput :Type])
                         :Value (val-vec task)
                         :Unit "mbar"}] }))

