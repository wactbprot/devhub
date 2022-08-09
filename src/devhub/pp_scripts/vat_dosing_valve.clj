(ns devhub.pp-scripts.vat-dosing-valve
  ^{:author "Thomas Bock <wactbprot@gmail.com>"
    :doc "Pre and Post scripts for VAT dosing valve."}
  (:require [devhub.pp-utils :as ppu]
            [clojure.string :as string]
            [devhub.utils :as u]))


;;------------------------------------------------------------
;; aux funs
;;------------------------------------------------------------
(defn int->pos-str
  "Turns the input into the command string (see
  also [[pos-str->int]]). If the input conversion fails `0`is used as
  input (which closes the valve).
  
  Example: ```clojure
  (int->pos-str 100)
  ;; => \"R:000100\\r\\n\"
  ```"
  [n]
  (format "R:%06d\r\n" (or (u/integer n) 0)))

(defn pos-str->int
  "Turns the position string into an integer (see also [[int->pos-str]]).

  Example: ```clojure
  (pos-str->int \"i:3800000000\")
  ;; => 0

  (pos-str->int \"i:3800000100\")
  ;; => 100  
  ```"
  [s] (-> s (subs 4) u/integer))

(defn position [{:keys [Position MaxSteps Unit]}]
  (if (and
       (= Unit "step")
       (int? Position)
       (pos? Position)
       (int? MaxSteps)
       (>=  MaxSteps Position))
    Position 0))


;;------------------------------------------------------------
;; prescripts
;;------------------------------------------------------------
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


;;------------------------------------------------------------
;; postscripts
;;------------------------------------------------------------
(defn get-position
  "Converts the result ov the position request to an integer by means
  of [[pos-str->int]]."
  [{x :_x :as task}]
  (assoc task :ToExchange
         {:PPCVATDosingValve {:Position (pos-str->int x)}}))

(defn ini
  "Replaces js pp. Furthermore, compares the length of the return vector
  `x` with the value vector `v`."
  [{x :_x v :Value :as task}]
  (if (= (count x) (count v))
    (assoc task :ToExchange
           {:PPCVATDosingValve {:MaxSteps 1000
                                :Mode "safe"
                                :Position 0}})
    (assoc task :error "one or more ini steps failed")))
