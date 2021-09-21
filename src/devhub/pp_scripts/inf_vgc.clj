(ns devhub.pp-scripts.inf-vgc
  ^{:author "Thomas Bock <wactbprot@gmail.com>"
    :doc "Post processing for a inficon vgc50x controller."}
  (:require [devhub.pp-utils :as ppu]
            [cheshire.core :as che]
            [devhub.utils    :as u]))

(def test-str "[1024.6, 1024.6, 1024.6]\n")

(defn read-out
  "The controller returns the pressure in `mbar` even if the return
  string looks like: `PA   5.07E-08`"
  [{{token :Type} :PostScriptInput x :_x {t :Type u :Unit} :PostScriptInput :as task}]
  (let [l (che/decode x)
        o (ppu/operable l)]
    (merge task {:Result (ppu/vl-result t (ppu/calc-seq l o) u)})))
