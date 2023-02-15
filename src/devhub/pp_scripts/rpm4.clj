(ns devhub.pp-scripts.rpm4
  ^{:author "Thomas Bock <wactbprot@gmail.com>"
    :doc "Post processing for Fluke/Europascal PPC/RPM4."}
  (:require [devhub.pp-utils :as ppu]
            [devhub.utils    :as u]
            [clojure.string :as string]))

(comment
  (def test-vec ["R        0.078 mbara"
                 "R        0.078 mbara"
                 "R       33.076 mbara"
                 "R   111111.079 mbara"
                 "R      222.078 mbara"])

  (mapv extract-value test-vec)
  ;; => [0.078 0.078 33.076 111111.079 222.078]
  (mapv extract-unit test-vec)
  ;; =>  ["mbar" "mbar" "mbar" "mbar" "mbar"]
 )

(defn extract [s]
  (let [r #"^R\s*([0-9]*\.[0-9]*)\s([a-zA-Z]*)a$"]
    (re-matches r s)))

(defn extract-value [s] (-> s extract second u/number))

(defn extract-unit [s] (-> s extract last))

(defn read-vec [{x :_x :as task}]
  (assoc task :Result [{:Type (get-in task [:PostScriptInput :Type])
                        :Value (mapv extract-value x)
                        :Unit (extract-unit (first x))}]))

(defn read-out [{x :_x :as task}]
  (assoc task :Result [(ppu/vl-result (get-in task [:PostScriptInput :Type])
                                       (->> (mapv extract-value x)
                                            (drop 2)
                                            vec)
                                       (extract-unit (first x)))]))
