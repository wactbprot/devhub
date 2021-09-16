(ns devhub.pp-scripts.mks670
  ^{:author "Thomas Bock <wactbprot@gmail.com>"
    :doc "Post processing for MKS controller Type 670."}
  (:require [devhub.pp-utils :as ppu]
            [devhub.utils :as u]))

(def s-test 
  {:_x ["@020 6.8368E-3","@020 6.8374E-3","@020 6.8320E-3", "@020 6.8308E-3",
        "@020 6.8317E-3","@020 6.8303E-3","@020 6.8318E-3",
        "@020 6.8305E-3","@020 6.8337E-3","@020 6.8276E-3","@020 6.8354E-3",
        "@020 6.8291E-3","@020 6.8262E-3","@020 6.8314E-3","@020 6.8293E-3",
        "@020 6.8290E-3","@020 6.8198E-3","@020 6.8262E-3","@020 6.8277E-3",
        "@020 6.8310E-3","@020 6.8303E-3","@020 6.8261E-3","@020 6.8213E-3",
        "@020 6.8272E-3","@020 6.8263E-3","@020 6.8238E-3","@020 6.8276E-3",
        "@020 6.8180E-3","@020 6.8180E-3","@020 6.8207E-3"],
 :_t_start ["1614683048753","1614683049349","1614683049945","1614683050540","1614683051145",
            "1614683051741","1614683052347","1614683052944","1614683053539","1614683054139",
            "1614683054733","1614683055327","1614683055919","1614683056515","1614683057111",
            "1614683057745","1614683058344","1614683058937","1614683059577","1614683060171",
            "1614683060767","1614683061379","1614683061997","1614683062595","1614683063193",
            "1614683063789","1614683064477","1614683065071","1614683065667","1614683066261"],
 :_t_stop ["1614683049049","1614683049645","1614683050239","1614683050845","1614683051441",
           "1614683052047","1614683052644","1614683053239","1614683053839","1614683054433",
           "1614683055027","1614683055619","1614683056215","1614683056811","1614683057445",
           "1614683058044","1614683058637","1614683059277","1614683059871","1614683060467",
           "1614683061079","1614683061697","1614683062295","1614683062893","1614683063489",
           "1614683064177","1614683064771","1614683065367","1614683065961","1614683066563"]})

(defn rs232-extract [s]
  (let [r #"@020\s([+-]*[0-9]{1,5}\.[0-9]{1,6}[E][-+][0-9]{1,3})"]
  (second (re-matches r s))))

(defn prologix-extract [s]
  (let [r #"MEASURING\s*([+-]*[0-9]{0,5}\.[0-9]{1,6}[E]*[-+]*[0-9]{0,3})"]
  (second (re-matches r s))))

(defn test-saw-tooth [{x :_x t0 :_t_start t1 :_t_stop :as task}]
  (let [v (mapv rs232-extract x)
        o (ppu/operable v)
        y (ppu/calc-seq v o)
        t (ppu/t0t1->t (ppu/calc-seq t0 o) (ppu/calc-seq t1  o))]
    (merge task {:ToExchange {:Pressure_decr {:Value (ppu/slope y t) :Unit "mbar/ms"}}})))

(defn saw-tooth {x :_x t0 :_t_start t1 :_t_stop :as task}
  (let [v (mapv rs232-extract x)
        o (ppu/operable v)
        y (ppu/calc-seq v o)
        t (ppu/t0t1->t (ppu/calc-seq t0 o)
                       (ppu/calc-seq t1 o))]
    (merge task {:Result [(ppu/vl-result "slope_x" (ppu/slope y t)    "mbar/ms")
                          (ppu/vl-result "R"       (ppu/r-square y t) "1")
                          (ppu/vl-result "mean_p"  (ppu/mean y)       "mbar")
                          (ppu/vl-result "mean_t"  (str (ppu/mean t))  "ms")
                          (ppu/vl-result "N"       (count y)          "1")]})))

(defn drift [{x :_x t0 :_t_start t1 :_t_stop :as task}]
  (let [infix (get-in  task [:PostScriptInput :Infix])
        v     (mapv rs232-extract x)
        o     (ppu/operable v)
        y     (ppu/calc-seq v o)
        t     (ppu/t0t1->t (ppu/calc-seq t0 o)
                           (ppu/calc-seq t1 o))]
     (merge task {:Result [(ppu/vl-result (str "drift_" infix "_slope_x") (ppu/slope y t)    "mbar/ms")
                           (ppu/vl-result (str "drift_" infix "_R")       (ppu/r-square y t) "1")
                           (ppu/vl-result (str "drift_" infix "_N")       (count y)          "1")]})))

(defn ctrl [task]
  (let [eps    (or (u/number (get-in  task [:PostScriptInput :Max_dev])) 0.005)
        p-trgt (or (u/number (get-in  task [:PostScriptInput :Pressure_target :Value])) 0.0001)
        v      (mapv prologix-extract (:_x task))
        p-curr (or (ppu/mean (ppu/calc-seq v (ppu/operable v))) 0.0)
        dp     (or (- (/ p-curr p-trgt) 1.0) 1.0)]
    (merge task {:ToExchange {:Filling_pressure_current {:Value p-curr 
                                                         :Unit "mbar"}
                              :Filling_pressure_dev {:Value dp 
                                                     :Unit "1"}
                              :Filling_pressure_ok {:Ready  (< (Math/abs dp) eps)}}})))
