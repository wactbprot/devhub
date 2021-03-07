(ns devhub.pp
  (:require [devhub.pp-scripts.daq34970  :as daq34970]
            [devhub.pp-scripts.gn-se3    :as gn-se3]
            [devhub.pp-scripts.vs-se3    :as vs-se3]
            [devhub.pp-scripts.servo-se3 :as servo-se3]
            [devhub.pp-scripts.im540     :as im540]
            [devhub.pp-scripts.vm212     :as vm212]
            [devhub.pp-scripts.mks670    :as mks670]))

(defn post-dispatch
  "TODO: make auto dispatch: can be done with ns-resolve; see:
  https://repl.it/@wactbprot/clj#main.clj
  
  Example:
  ```clojure
  ;; e.g.:
  (keys (ns-publics 'devhub.pp-scripts.vs_se3))
  ;; =>
  ;; (conf registers-ok? check valves switches)
  ```"
  [conf task]
  (let [ps (keyword (:PostScript task))]
    (condp = ps
      :daq34970.temperature-scanner-read-out (daq34970/temperature-scanner-read-out task)
      :vm212.dcr-read-out                    (vm212/dcr-read-out                    task)
                                                                                    
      :mks670.test-saw-tooth                 (mks670/test-saw-tooth                 task)
      :mks670.saw-tooth                      (mks670/saw-tooth                      task)
      :mks670.drift                          (mks670/drift                          task)
                                                                                    
      :servo-se3.meas-velo                   (servo-se3/meas-velo                   task)
      :servo-se3.resp-ok                     (servo-se3/resp-ok                     task)
      :servo-se3.set-velo                    (servo-se3/set-velo                    task)
      :servo-se3.get-pos                     (servo-se3/get-pos                     task)
                                                                                    
      :vs_se3.valves                         (vs-se3/valves                         task)
      :vs_se3.switches                       (vs-se3/switches                       task)
                                                                                    
      :gn_se3.anybus-readout                 (gn-se3/anybus-readout                 task)
      :gn_se3.anybus-pressure-ctrl           (gn-se3/anybus-pressure-ctrl           task)
      :gn_se3.anybus-add-ctrl                (gn-se3/anybus-add-ctrl                task)
      :gn_se3.anybus-add-read                (gn-se3/anybus-add-read                task)
      :gn_se3.anybus-add-loss                (gn-se3/anybus-add-loss                task)
                                                                                    
      :im540.read-out                        (im540/read-out                        task)
      :im540.pressure-rise                   (im540/pressure-rise                   task)
      {:error (str "no :PostScript named: " ps)})))


(defn pre-dispatch
  [conf task]
  (let [ps (keyword (:PreScript task))]
    (condp = ps
      :vs_se3.set-valve (vs-se3/set-valve task)
      {:error (str "no :PreScript named: " ps)})))
    
