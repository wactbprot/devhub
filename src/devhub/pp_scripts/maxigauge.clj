(ns devhub.pp-scripts.maxigauge
  ^{:author "Thomas Bock <wactbprot@gmail.com>"
    :doc "Post processing for Maxigauge and DualGauge controller."}
  (:require [clojure.string :as string]
            [devhub.pp-utils :as ppu]
            [devhub.utils :as u]))

(def test-vec [""
               "0,-2.4342E+00"
               ""
               "0,+9.4771E-05"
               ""
               "0,+1.4055E-03"
               ""
               "0,-1.4192E-04"
               ""
               "0,+4.5151E-04"])

(def test-input {:CH1 {:Reservoir 3, :Fullscale 1000, :Unit "mbar"},
                 :CH2 {:Reservoir 3, :Fullscale 10, :Unit "mbar"},
                 :CH3 {:Reservoir 4, :Fullscale 10, :Unit "mbar"},
                 :CH4 {:Reservoir 4, :Fullscale 0.1, :Unit "mbar"},
                 :CH5 {:Reservoir 5, :Fullscale 10, :Unit "mbar"},
                 :CH6 {:Reservoir 5, :Fullscale 0.1, :Unit "mbar"}})

(defn rm-ack [v] (filterv #(< 1 (count %)) v))

(defn extract-value
  "Extracts the value from the given string."
  [s]
  (let [v (string/split s #",")]
    (when (= (count v) 2)
      (-> v second u/number))))

(defn read-out [{{t :Type u :Unit} :PostScriptInput x :_x :as task}]
  (let [v (mapv extract-value x)
        r (ppu/vl-result t v u)]
    (merge task {:Result r})))
 

(defn read-all [{input :PostScriptInput x :_x :as task}]
  (merge task
         {:ToExchange (into {}
                            (mapv (fn [[k v] w] {k (assoc v :Value w)})
                                  input
                                  (mapv extract-value (rm-ack x))))}))

(defn safe-value
  "Extracts the value of the safe channel.

  Example:
  ```clojure
  (safe-value [\"\" \"0,+1.0200E+03\"])
  ;; => 1020.0
  ```"
  [v]
  (-> v
      rm-ack
      first
      extract-value))

(defn pressure-safe? [{{:keys [TargetPressure TargetUnit]} :PostScriptInput x :_x}]
  (let [current-pressure (safe-value x)
        max-pressure (* (or (u/number TargetPressure) 0.0) (+ 1.1))]
    (cond
      (zero? max-pressure) false
      (nil? current-pressure) false
      (>= current-pressure max-pressure) false
      (< current-pressure max-pressure) true)))
      
(defn safe
  "Uses the readout to ensure that the
  pressure is below a certain pressure value. If the pressure `p` is greater than
  `(* p (+ 1.0 MaxOverShoot)` the `:Mode` of the PPC VAT Dosing Valve is set to `safe`
  which will trigger the valve closing."
  [{{:keys [TargetPressure TargetUnit]} :PostScriptInput x :_x :as task}]
  (let [current-pressure (safe-value x)]
    (assoc task :ToExchange
           {:ObservePressure {:Value current-pressure :Unit TargetUnit}
            :PPCVATDosingValve (if (pressure-safe? task) 
                                 {:Ok true  :Mode "auto"}
                                 {:Ok false :Mode "safe" })})))

(comment
  (def x ["" "0,+4.3300E-02" "" "0,+4.3300E-02" "" "0,+4.3300E-02" "" "0,+4.3300E-02"]))

(defn slope [{x :_x t0 :_t_start t1 :_t_stop}]
  (let [v (mapv extract-value (rm-ack x))
        o (ppu/operable v)
        y (ppu/calc-seq v o)
        t (ppu/t0t1->t (ppu/calc-seq t0 o) (ppu/calc-seq t1 o))]
    (ppu/slope y t)))

(defn next-pos [{{:keys [PPCVATDosingValvePos PPCVATDosingMaxSteps]} :PostScriptInput}]
  (let [curr-pos (u/number PPCVATDosingValvePos)
        max-pos (u/number (or PPCVATDosingMaxSteps 1000))
        care-pos 450
        fine-steps 10
        coarse-steps 100
        steps (if (> curr-pos care-pos) fine-steps coarse-steps)
        pos (+ curr-pos steps)]
    (if (> pos max-pos) max-pos pos)))

(defn ms-to-target [{{:keys [TargetPressure PPCVATDosingValveMode ]} :PostScriptInput :as task}]
  (let [target-pressure (u/number TargetPressure)
        relax-time 20000.
        react-time 2000.
        m (slope task) ; mbar/ms
        t (when-not (zero? m) (/ target-pressure m))]
    (cond
      (nil? t) relax-time
      (neg? t) relax-time
      (> react-time t) 0
      (> relax-time t react-time) (do
                                    (Thread/sleep (- t react-time))
                                    0)
      :default t)))

(defn ctrl
  "Returns `task` (without any ops) if `PPCVATDosingValveMode` is not `auto`."
  [{{:keys [TargetPressure PPCVATDosingValveMode ]} :PostScriptInput :as task}]
  (if (= PPCVATDosingValveMode "auto")
    (let [t (ms-to-target task)]
      (assoc task :ToExchange (if (zero? t)
                                {:PPCVATDosingValve {:Position 0}}
                                {:PPCVATDosingValve {:Position (next-pos task)}})))
    task))
