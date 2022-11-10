(ns devhub.pp-scripts.inf-bcg450
  ^{:author "Thomas Bock <wactbprot@gmail.com>"
    :doc "Post processing for Inficon binary protocol."}
  (:require [devhub.pp-utils :as ppu]
            [devhub.utils    :as u]))

(def conf {:start-vec [7 5]
           :meas-offset 4
           :status-offset 2
           :mult-high 256
           :div-by 4000
           :mbar 12.5
           :Torr 12.625
           :Pa 10.5})
(comment
  (def v [7 5 17 0 128 3 62 13 228])
  ;; byte 2 (status byte) is 17
  ;; which means the unit is Torr
  ;; https://www.inficon.com/v1/attachment/93fc640e-3569-4dbb-84d3-e9d61ec9d230/TINA40D1%20Triple%20Gauge%20BCG450%20Gebrauchsanleitung_lang.pdf
    )

(defn vec->p-vec [v] (mapv (fn [x y] [x y]) v (rest v)))

(defn p-vec->start-idx [v] (.indexOf v (-> conf :start-vec)))

(defn p-vec->meas-vec [p i] (nth p (+ i (-> conf :meas-offset))))

(defn p-vec->status-byte [p i]
  (-> p
      (nth (+ i (-> conf :status-offset)))
      first))

(defn status-byte->unit
  "Filters bit 4 and 5 (count from 0, 48 = 110000).
  Unit according  p. 38"
  [b]
  (condp = (bit-and b 48)
    0 "mbar"
    16 "Torr"
    32 "Pa"))

(defn meas-vec->meas-val
  "Calculation according to manual p. 38" 
  [[h l] u]
  (let [s (keyword u)]
    (Math/pow 10 (- (/ (+ 
                        (* h (-> conf :mult-high)) l)
                       (-> conf :div-by))
                    (-> conf s)))))
  
(defn meas-unit [v]
  (let [p (vec->p-vec v)
        i (p-vec->start-idx p)]
    (-> p
        (p-vec->status-byte i)
        status-byte->unit)))
  
(defn meas-val [v]
  (let [p (vec->p-vec v)
        i (p-vec->start-idx p)
        u (meas-unit v)]
    (-> p
        (p-vec->meas-vec i)
        (meas-vec->meas-val u))))

(defn readout [{x :_x :as task}]
  (let [v (mapv meas-val x)
        u (-> x first meas-unit)]
    (merge task {:Result [(ppu/vl-result "ind" v u)]})))
