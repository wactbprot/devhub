(ns devhub.pp-scripts.utils
  (:require [clojure.edn        :as edn]
            [jdk.nio.ByteBuffer :as bb]))

(defn open? [r n] (bit-test r n))

(defn first-key [m] (when (map? m) (first (keys m))))
(defn first-val [m] (when (map? m) (first (vals m))))

(defn bool->exch-map [b] (if b {:Bool true} {:Bool false})) 

(defn exch-bool-map
  [vs]
  (mapv
   (fn [m] {(first-key m) (bool->exch-map (first-val m))})
   vs)) 


(defn b16->h [b] (bit-and (bit-shift-right b 8) 0xff))

(defn b16->l [b] (bit-and  b 0xff))

(defn vec->float [v] (-> v byte-array bb/*wrap bb/get-float))
