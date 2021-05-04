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

(defn readout-first
  "Returns `Result` and `ToExchange` maps.
  
  NOTE: The kunbus gateway is configured to deliver pressures in `mbar`."
  [task]
  (let [s (get-in task [:PostScriptInput :Type])
        v (:_x task)]
    (merge task
           {:Result (ppu/vl-result s
                                   (mapv  #(ppu/vec->float (kunbus-vec %)) v)
                                   "mbar") })))
  
