(ns devhub.pp-scripts.mks627-kunbus
  (:require [devhub.pp-utils :as ppu]
            [devhub.utils    :as u]
            [com.brunobonacci.mulog :as µ]))

(comment
  (def v [0 0 0 16128 2836 120 0 0]) ; 0.578125 mbar)
  (def v [0 0 0 16640 3905 144 0 0]); 12.0625 mbar)
  )

(defn kunbus-vec
  "Returns a vector of  bytes that can be turned into a float.

  Example:
  ```clojure
  (kunbus-vec  [0 0 0 16128 2836 120 0 0])
  ;; =>
  ;; [63 20 0 0] ; 0.578125 mbar
  ```"
  [v]
  [(ppu/b16->h (nth v 3))
   (ppu/b16->l (nth v 4))
   (ppu/b16->h (nth v 5))
   0])

(defn val-vec [task] (mapv #(ppu/vec->float (kunbus-vec %)) (:_x task)))

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

