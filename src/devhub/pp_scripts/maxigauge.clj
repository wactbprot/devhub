(ns devhub.pp-scripts.maxigauge
  ^{:author "Thomas Bock <wactbprot@gmail.com>"
    :doc "Post processing for Maxigauge and DualGauge controller."}
  (:require [clojure.string :as string]
            [devhub.pp-utils :as ppu]
            [devhub.utils :as u]))

;;------------------------------------------------------------
;; configuration
;;------------------------------------------------------------
(def config
  "Configuration map defines values used e.g. at pressure control tasks."
  {:overshoot 0.1 ;; rel. value 
   :undershoot 0.05 ;; rel. value 

   ;; VAT dosing valve  
   :max-pos 1000 ;; steps
   :care-pos 500 ;; steps
   :super-care-pos 560 ;; steps

   :coarse-steps 100 ;; steps
   :fine-steps 5 ;; steps
   :super-fine-steps 2 ;; steps

   :relax-time 30000. ;; ms
   })

;;------------------------------------------------------------
;; aux funs
;;------------------------------------------------------------
(defn rm-ack [v] (filterv #(< 1 (count %)) v))

(defn extract-value
  "Extracts the value from the given string."
  [s]
  (let [v (string/split s #",")]
    (when (= (count v) 2)
      (-> v second u/number))))

(defn gen-vec [x] (mapv extract-value (rm-ack x)))

(defn last-pressure-value
  "Extracts the value of the safe channel.

  Example:
  ```clojure
  (last-pressure-value [\"\" \"0,+1.0200E+03\"])
  ;; => 1020.0
  ```"
  [v]
  (-> v rm-ack last extract-value))

(defn pressure-safe?
  ([current-pressure target-pressure]
   (pressure-safe? current-pressure target-pressure config))
  ([current-pressure target-pressure {:keys [overshoot] :as conf}]
   (let [max-pressure (* (or target-pressure  0.0) (+ 1.0 overshoot))]
     (cond
       (zero? max-pressure)               false
       (nil? current-pressure)            false
       (>= current-pressure max-pressure) false
       (< current-pressure max-pressure)  true))))
  
(defn pressure-ok?
  ([current-pressure target-pressure]
   (pressure-ok? current-pressure target-pressure config))
  ([current-pressure target-pressure {:keys [overshoot undershoot] :as conf}]
   (let [upper-pressure (* (or target-pressure  0.0) (+ 1.0 overshoot))
         lower-pressure (* (or target-pressure  0.0) (- 1.0 undershoot))]
     (> upper-pressure current-pressure lower-pressure))))


(defn slope [{x :_x t0 :_t_start t1 :_t_stop}]
  (let [v (gen-vec x)
        o (ppu/operable v)
        y (ppu/calc-seq v o)
        t (ppu/t0t1->t (ppu/calc-seq t0 o) (ppu/calc-seq t1 o))]
    (ppu/slope y t)))

(defn next-pos
  ([curr-pos]
   (next-pos curr-pos config))
  ([curr-pos {:keys [max-pos care-pos super-care-pos fine-steps super-fine-steps coarse-steps] :as conf}]
   (let [steps (cond
                 (< curr-pos care-pos) coarse-steps
                 (< curr-pos super-care-pos) fine-steps
                 :default super-fine-steps) 
         pos (+ curr-pos steps)]
     (if (>= pos max-pos) max-pos pos))))

(defn position
  ([task]
   (position task config))
  ([{{:keys [TargetPressure PPCVATDosingValvePos]} :PostScriptInput x :_x :as task} {:keys [relax-time] :as conf}]
   (let [curr-pos (u/number PPCVATDosingValvePos)
         target-pressure (u/number TargetPressure)
         current-pressure (last-pressure-value x)
         pressure-rise (slope task)
         remain-time (when-not (zero? pressure-rise) (/ target-pressure pressure-rise))]
     (cond
       (pressure-ok? current-pressure target-pressure) 0
       (or (nil? remain-time)
           (neg? remain-time))                         (next-pos curr-pos)
       (> relax-time remain-time)                      curr-pos
       :default                                        (next-pos curr-pos)))))


;;------------------------------------------------------------
;; safe
;;------------------------------------------------------------
(defn safe
  "Checks if the pressure extracted by [[last-pressure-value]] from
  `x` is safe (see [[pressure-safe?]] or ok (see [[pressure-ok?]])
  with respect to the `TargetPressure` provided via
  `:PostScriptInput`."
  [{{:keys [TargetPressure TargetUnit]} :PostScriptInput x :_x :as
  task}]
  (let [current-pressure (last-pressure-value x)
        target-pressure (u/number TargetPressure)
        p? (pressure-ok? current-pressure target-pressure)
        s? (pressure-safe? current-pressure target-pressure)]
    (assoc task :ToExchange
           {:Continue_setting {:Bool (not p?)}
            :ObservePressure {:Value current-pressure :Unit TargetUnit}
            :PPCVATDosingValve (cond
                                 p? {:Mode "done"} 
                                 s? {:Mode "auto"} 
                                 :else {:Mode "safe"})})))


;;------------------------------------------------------------
;; ctrl
;;------------------------------------------------------------
(defn ctrl
  "Returns `task` (without any ops) if `PPCVATDosingValveMode` is not `auto`.
  If `PPCVATDosingValveMode` is `auto` sets the `:PPCVATDosingValve`
  `:Position` by means of [[position]]."
  [{{:keys [PPCVATDosingValveMode]} :PostScriptInput :as task}]
  (if (= PPCVATDosingValveMode "auto")
    (assoc task :ToExchange
           {:PPCVATDosingValve {:Position (position task)}})
    task))


;;------------------------------------------------------------
;; read-out
;;------------------------------------------------------------
(defn read-out
  "Maps [[extract-value]] over the result vector `x`. Returns with
  VacLab result format."
  [{{t :Type u :Unit} :PostScriptInput x :_x :as task}]
  (let [v (gen-vec x)]
    (assoc task :Result [(ppu/vl-result t v u)])))
 

;;------------------------------------------------------------
;; read-all
;;------------------------------------------------------------
(defn read-all [{input :PostScriptInput x :_x :as task}]
  (assoc task :ToExchange (into {} (mapv (fn [[k v] w] {k (assoc v :Value w)})
                                         input
                                         (gen-vec x)))))

;;------------------------------------------------------------
;; read-vector
;;------------------------------------------------------------
(defn coll-channel [n v]
  (mapv #(second %) (filter #(= (first %) n) v)))

;; alternatives
;; (def vv (group-channel ...))
;; (group-by first vv)
;; or 
;; (reduce (fn [m v] (update-in m [(first v)] conj (second v))) {} vv)
;;
(defn group-channel [v m]
  (mapv (fn [x i] [i x]) v (cycle (range m))))

(defn read-vector [{{ts :Time_start tk :Token u :Unit} :PostScriptInput x :_x :as task}]
  (let [v (-> x
              (gen-vec)
              (group-channel 6))]
    (assoc task :Result [{:Type (str tk "_ch1_" ts) :Value (coll-channel 0 v) :Unit u}
                         {:Type (str tk "_ch2_" ts) :Value (coll-channel 1 v) :Unit u}
                         {:Type (str tk "_ch3_" ts) :Value (coll-channel 2 v) :Unit u}
                         {:Type (str tk "_ch4_" ts) :Value (coll-channel 3 v) :Unit u}
                         {:Type (str tk "_ch5_" ts) :Value (coll-channel 4 v) :Unit u}
                         {:Type (str tk "_ch6_" ts) :Value (coll-channel 5 v) :Unit u}])))
