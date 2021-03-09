(ns devhub.pp-scripts.mkspr4000
  (:require [devhub.pp-utils :as ppu]
            [devhub.utils    :as u]))

(defn ctrl-str [m] (str "SP1," (:sp1 m)"\rSP2," (:sp2 m) "\r"))

(defn calq
  [task]
  (let [dev     (or (u/number (get-in  task [:PreScriptInput :Filling_Pressure_Dev :Value])) 0.0001)
        p-trgt  (or (u/number (get-in  task [:PreScriptInput :Pressure_target :Value])) 1.0001)
        ;; [mq] = mbar/s/sccm 0.018 passt ~ für SE1 und FM3
        ;; FM3 war:  mq = Vopen ? 0.0133 : 0.0155,
        MQ      (or (u/number (get-in  task [:PreScriptInput :MQ])) 0.018) 
        ts       20.0 ;; Sollzeit (p wird theor. in ts erreicht)
        glim     0.7  ;; Start Regelung rel. Abw. von psoll (g ...general)
        slim     2.0  ;; Start 2. Regler (s... second)
        md       1.0  ;; Dämpf./verst. der Regel.
        q        (/ p-trgt ts MQ)
        f        (* md dev)
        m     (cond
                (and  (>= glim dev)  (> slim    (* f q))) {:sp1 (* f q) :sp2 0}
                (and  (>= glim dev)  (> (* f q) slim))    {:sp1 (* f q) :sp2 (/ (* f q) 2)}
                (and  (> dev   glim) (> q       slim))    {:sp1 q       :sp2 (/ q 2)}
                (and  (> dev   glim) (> slim    q))       {:sp1 q       :sp2 0}
                :else                                     {:sp1 0.0     :sp2 0.0})]
    (assoc task :Value [(ctrl-str m)])))
