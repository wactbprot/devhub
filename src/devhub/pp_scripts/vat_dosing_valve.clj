(ns devhub.pp-scripts.vat-dosing-valve
  ^{:author "Thomas Bock <wactbprot@gmail.com>"
    :doc "Pre and Post scropts for VAT valve."}
  (:require [devhub.pp-utils :as ppu]
            [clojure.string :as string]
            [devhub.utils :as u]))

(defn int->pos-str [n] (format "R:%06d\r\n" n))

(defn position [{:keys [Position MaxSteps Unit]}]
  (if (and
       (= Unit "step")
       (int? Position)
       (pos? Position)
       (int? MaxSteps)
       (>=  MaxSteps Position))
    Position 0))

(defn follow
  "Turns the `:PreScriptInput` into a `Value` to send.
    
  ```clojure
  (def input {:MaxSteps 1000
              :Mode \"auto\"
              :Position 1000,
              :Unit \"step\"})
  ```"
  [{{:keys [Mode] :as input} :PreScriptInput :as task}]
  (let [pos (if (= Mode "auto") (position input) 0)]
    (assoc task
           :Value (int->pos-str pos)
           :ToExchange {:PPCVATDosingValve {:Position pos}})))
    
