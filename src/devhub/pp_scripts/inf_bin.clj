(ns devhub.pp-scripts.inf-bin
  ^{:author "Thomas Bock <wactbprot@gmail.com>"
    :doc "Post processing for Inficon binary protocol."}
  (:require [devhub.pp-utils :as ppu]
            [devhub.utils    :as u]))

(def conf {:start-vec [7 5]
           :meas-offset 4
           :mult-high 256
           :div-by 4000
           :subs-mbar 12.5})

(defn vec->p-vec [v]
  (mapv (fn [x y] [x y]) v (rest v)))

(defn p-vec->start-idx [v]
  (.indexOf v (-> conf :start-vec)))

(defn p-vec->meas-vec [p i]
  (nth p (+ i (-> conf :meas-offset))))

(defn meas-vec->meas-val
  "Calculation according to manual p. 38" 
  [[h l]]
    (Math/pow 10 (- (/ (+ 
                        (* h (-> conf :mult-high)) l)
                       (-> conf :div-by))
                    (-> conf :subs-mbar))))
  
(defn meas-val [v]
  (let [p (vec->p-vec v)
        i (p-vec->start-idx p)]
    (-> p
        (p-vec->meas-vec i)
        (meas-vec->meas-val))))

(defn readout [{x :_x :as task}]
  (let [v (mapv meas-val x)]
    (merge task {:Result [(ppu/vl-result "ind" v "mbar")]})))
