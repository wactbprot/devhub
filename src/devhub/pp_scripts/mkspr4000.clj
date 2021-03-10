(ns devhub.pp-scripts.mkspr4000
  (:require [devhub.pp-utils :as ppu]
            [devhub.utils    :as u]))

(defn ctrl-str [{sp1 :sp1 sp2 :sp2}] (str "SP1," sp1 "\rSP2," sp2 "\r"))

(defn cut-fs
  [{sp1 :sp1 sp2 :sp2 :as m} {fs1 :fs1 fs2 :fs2}]
  (assoc m
         :sp1 (if (> sp1 fs1) fs1 sp1)
         :sp2 (if (> sp2 fs2) fs2 sp2)))

(defn calq
  [{mq :mq ts :ts glim :glim slim :slim md :md p :p dev :dev :as m}]
  (let [q (/ p ts mq)
        f (* md (Math/abs dev))]
    (cond
      (and  (>= glim dev)  (> slim    (* f q))) {:sp1 (* f q) :sp2 0.0}
      (and  (>= glim dev)  (> (* f q) slim))    {:sp1 (* f q) :sp2 (/ (* f q) 2)}
      (and  (> dev   glim) (> q       slim))    {:sp1 q       :sp2 (/ q 2)}
      (and  (> dev   glim) (> slim    q))       {:sp1 q       :sp2 0.0}
      :else                                     {:sp1 0.0     :sp2 0.0})))

(defn calq-fm3
  "Pre calculation of the flow `q` setpoints `sp1` and `sp2` to reach the target
  pressure `p` in `ts` seconds. Pressures are in mbar.
  
  Example:
  ```clojure
  (ctrl-str
     (cut-fs
        (calq {:p 27, :dev -1.0, :mq 0.018, :ts 20.0, :glim 0.7, :slim 1.0, :md 1.0})
     {:fs1 1.0 :fs2 100.0}))  
  ```"
  [task]
  (let [dev     (or (u/number (get-in  task [:PreScriptInput :Filling_Pressure_Dev :Value])) 0.0001)
        p       (or (u/number (get-in  task [:PreScriptInput :Pressure_target :Value])) 0.0001)
        u       (or (get-in  task [:PreScriptInput :Pressure_target :Unit]) "mbar")
        fs      {:fs1 1.0 :fs2 100.0}
        param   {:p    p      ;; target pressure 
                 :unit u      ;; target pressure unit
                 :dev  dev    ;; current deviation
                 ;;              [mq] = mbar/s/sccm 0.018 is ok for SE1 and FM3
                 :mq   0.018  ;; FM3:  mq = Vopen ? 0.0133 : 0.0155, 
                 :ts   20.0   ;; theoretical time to reach p
                 :glim 0.7    ;; fast forward limit
                 :slim 1.0    ;; limit of use of second controller 
                 :md   1.0} ] ;; damping/amp. factor
    (assoc task :Value [(ctrl-str (cut-fs (calq param) fs))])))
