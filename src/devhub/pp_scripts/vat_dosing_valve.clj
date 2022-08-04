(ns devhub.pp-scripts.vat-dosing-valve
  ^{:author "Thomas Bock <wactbprot@gmail.com>"
    :doc "Pre and Post scropts for VAT valve."}
  (:require [devhub.pp-utils :as ppu]
            [clojure.string :as string]
            [devhub.utils :as u]))

(defn int->pos-str
  "Turns an integer to the command string.
  
  Example:
  '''clojure
  (int->pos-str 100)
  ;; => \"R:000100\\r\\n\"
  ```"
  [n] (format "R:%06d\r\n" n))

(defn pos-str->int
  "Turns the position string to an integer.

  Example:
  '''clojure
  (pos-str->int \"i:3800000000\")
  ;; => 0

  (pos-str->int \"i:3800000100\")
  ;; => 100  
  ```"
  [s] (-> s (subs 4) Integer/parseInt))
  
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
    (assoc task :Value (int->pos-str pos))))
    
(defn get-position [{x :_x :as task}]
  (assoc task :ToExchange
         {:PPCVATDosingValve {:Position (pos-str->int x)}}))
