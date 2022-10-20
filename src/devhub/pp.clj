(ns devhub.pp
^{:author "Thomas Bock <wactbprot@gmail.com>"
    :doc "Dispatch pp scripts.
         TODO: make auto dispatch: can be done with ns-resolve; see:
          https://repl.it/@wactbprot/clj#main.clj"}
   (:require [devhub.pp-scripts.daq34970 :as daq34970]
             [devhub.pp-scripts.gn-se3 :as gn-se3]
             [devhub.pp-scripts.vs-se3 :as vs-se3]
             [devhub.pp-scripts.servo-se3 :as servo-se3]
             [devhub.pp-scripts.servo-ppc :as servo-ppc]
             [devhub.pp-scripts.im540 :as im540]
             [devhub.pp-scripts.inf-cube :as inf-cube]
             [devhub.pp-scripts.inf-vgc :as inf-vgc]
             [devhub.pp-scripts.vm212 :as vm212]
             [devhub.pp-scripts.frs5 :as frs5]
             [devhub.pp-scripts.maxigauge :as maxigauge]
             [devhub.pp-scripts.mks670 :as mks670]
             [devhub.pp-scripts.mks-srg3 :as mks-srg3]
             [devhub.pp-scripts.mkspr4000 :as mkspr4000]
             [devhub.pp-scripts.vat-dosing-valve :as vat-dosing-valve]
             [devhub.pp-scripts.mks627-kunbus :as mks627-kunbus]
             [devhub.pp-scripts.vacom :as vacom]
             [devhub.pp-scripts.keithley :as keithley]
             
             [com.brunobonacci.mulog :as µ]))


(defn post-dispatch
  "Example:
  ```clojure
  ;; e.g.:
  (keys (ns-publics 'devhub.pp-scripts.vs_se3))
  ;; =>
  ;; (conf registers-ok? check valves switches)
  ```"
  [conf {ps :PostScript x :_x :as task}]
  (µ/log ::post-dispatch :message "exec post script" :raw-result-str x)
    (condp = (keyword ps)
      :daq34970.temperature-scanner-read-out (daq34970/temperature-scanner-read-out task)

      :vm212.dcr-read-out                    (vm212/dcr-read-out                    task)

      :frs5.lb-read-out                      (frs5/lb-read-out                      task)

      :mks670.test-saw-tooth                 (mks670/test-saw-tooth                 task)
      :mks670.saw-tooth                      (mks670/saw-tooth                      task)
      :mks670.drift                          (mks670/drift                          task)
      :mks670.ctrl                           (mks670/ctrl                           task)

      :mks-srg3.read-out                     (mks-srg3/read-out                     task)
      :mks-srg3.read-with-slope              (mks-srg3/read-with-slope              task)
      :mks-srg3.read-freq                    (mks-srg3/read-freq                    task)
      :mks-srg3.read-sample                  (mks-srg3/read-sample                  task)
      :servo-se3.meas-velo                   (servo-se3/meas-velo                   task)
      :servo-se3.resp-ok                     (servo-se3/resp-ok                     task)
      :servo-se3.set-velo                    (servo-se3/set-velo                    task)
      :servo-se3.get-pos                     (servo-se3/get-pos                     task)

      :servo-ppc.meas-velo                   (servo-ppc/meas-velo                   task)
      :servo-ppc.get-pos                     (servo-ppc/get-pos                     task)
      :servo-ppc.ini                         (servo-ppc/ini                         task)

      :vs_se3.valves                         (vs-se3/valves                         task)
      :vs_se3.switches                       (vs-se3/switches                       task)

      :gn_se3.anybus-readout                 (gn-se3/anybus-readout                 task)
      :gn_se3.anybus-pressure-ctrl           (gn-se3/anybus-pressure-ctrl           task)
      :gn_se3.anybus-add-ctrl                (gn-se3/anybus-add-ctrl                task)
      :gn_se3.anybus-add-read                (gn-se3/anybus-add-read                task)
      :gn_se3.anybus-add-loss                (gn-se3/anybus-add-loss                task)

      :im540.read-out                        (im540/read-out                        task)
      :im540.pressure-rise                   (im540/pressure-rise                   task)
      :inf-cube.readout                      (inf-cube/readout                      task)
      :inf-cube.readout-vec                  (inf-cube/readout-vec                  task)
      :maxigauge.read-out                    (maxigauge/read-out                    task)
      :maxigauge.read-all                    (maxigauge/read-all                    task)
      :maxigauge.read-vector                 (maxigauge/read-vector                 task)

      :maxigauge.safe                        (maxigauge/safe                        task)
      :maxigauge.ctrl                        (maxigauge/ctrl                        task)

      :mks627-kunbus.readout-first           (mks627-kunbus/readout-first           task)
      :mks627-kunbus.readout-first-vec       (mks627-kunbus/readout-first-vec       task)

      :vacom.check-response                  (vacom/check-response                   task)
      :vacom.read-pressure                   (vacom/read-pressure                    task)
      :vacom.read-pressure-vec               (vacom/read-pressure-vec                task)

      :keithley.read-6485-coll-current       (keithley/read-6485-coll-current        task)
      
      :vat-dosing-valve.get-position         (vat-dosing-valve/get-position          task)
      :vat-dosing-valve.ini                  (vat-dosing-valve/ini                   task)

      :inf-vgc.read-out                      (inf-vgc/read-out                       task)
      :inf-vgc.read-vec                      (inf-vgc/read-vec                       task)
      {:error (str "no :PostScript named: " ps)}))

(defn pre-dispatch [conf {ps :PreScript :as task}]
  (µ/log ::pre-dispatch :message "exec pre script")
  (condp = (keyword ps)
    :vacom.meas-pressure      (vacom/meas-pressure  task)
    :mkspr4000.calq-fm3       (mkspr4000/calq-fm3   task)
    :vs_se3.set-valve         (vs-se3/set-valve     task)
    :vat-dosing-valve.follow  (vat-dosing-valve/follow     task)
    {:error (str "no :PreScript named: " ps)}))
