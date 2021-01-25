(ns devhub.pp-scripts.im540
  (:require [devhub.pp-utils :as ppu]
            [devhub.utils    :as u]))

(def test-vec ["MES R" "MES R" "MES R" "PA   0.00E+00" 
               "PA   5.07E-08" "MES R" "PA   0.00E+00" 
               "MES R" "MES R" "MES R"])

(defn extract
  [s]
  (let [r #"[PA]*\s*([0-9]{1}\.[0-9]{1,2}[E][-+][0-9]{2})"]
  (second (re-matches r s))))

(defn read-out
  "The controller returns the pressure in `mbar` even if the return
  string looks like: `PA   5.07E-08`"
  [task]
  (let [input (:PostScriptInput task)
        v     (mapv extract (:_x task))
        o     (ppu/operable v)]
    (ppu/vl-result (:Type input) (ppu/calc-seq v o) "mbar")))

(defn pressure-rise
  [task]
  (let [input (:PostScriptInput task)
        token (:Type input)
        v     (mapv extract (:_x task))
        o     (ppu/operable v)
        y     (ppu/calc-seq v o)
        t     (ppu/t0t1->t (ppu/calc-seq (:_t_start task) o)
                           (ppu/calc-seq (:_t_stop task)  o))]
    (merge task {:Result [(ppu/vl-result (str token "_slope_x")
                                         (ppu/slope y t)
                                         "mbar/ms")
                          (ppu/vl-result (str token "_R")
                                         (ppu/r-square y t)
                                         "1")
                          (ppu/vl-result (str token "_N")
                                         (count y)
                                         "1")]})))
        
        
