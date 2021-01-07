(ns devhub.pp-scripts.gn_se3
  (:require [devhub.pp-scripts.utils :as ppu]
            [devhub.utils            :as u]))

(def conf (u/config "gn_se3.edn"))

(defn anybus-vec
  "Returns a vector of 4 bytes that can be turned into a float.
  ```clojure
  (anybus-vec 32832,99,46336,257)
  ;; =>
  ;; [64 0 99 181]
  ```"
  [v]
  [(ppu/b16->l (nth v 0))
   (ppu/b16->h (nth v 1))
   (ppu/b16->l (nth v 1))
   (ppu/b16->h (nth v 2))])

(defn anybus-float [v s n] (ppu/vec->float (anybus-vec (subvec v s (+ s n)))))

(defn anybus-readout
  "Returns `Result` and `ToExchange` maps.
  
  ```clojure
  ;; approx. 200Pa:
  (def m {:_x [32825,23151,30976,257,32826,17559,34304,257,32826,6375,41472,257,
               32832,161,9216,257,32832,120,12032,257,32832,161,9216,257,
               32832,338,40448,257,32832,202,6400,257,32832,338,40448,257,
               32831,65530,41216,257,32831,61434,63232,257,32831,65530,41216,257,
               32832,150,59136,257,32832,31,29184,257,32831,63482,52224,257,
               32832,99,46336,257]
  :_t_start 2})
  (anybus-readout conf m) 
  ```"
  [task data]
  (let [m (:anybus-byte-start conf) n (:anybus-byte-count conf)
        x (:_x data)
        f (fn [[dev-name start]]
            {dev-name (if (u/single-meas? data)
                        (anybus-float x start n)
                        (mapv (fn [v] (anybus-float v start n)) x))})] 
    
    (mapv f m)))
