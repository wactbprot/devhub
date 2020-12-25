(ns devhub.py-pp
  (:require [clojure.string     :as string]
            [cheshire.core      :as che]
            [clojure.java.shell :refer [sh]]
            [devhub.utils       :as u]
            [com.brunobonacci.mulog :as µ]))

(defn pp-script [conf task] (str (:py-path conf)  "/" (:PostScriptPy task)  ".py"))

(defn exec
  "Executes a predefined *python3* script given with `:PostScriptPy`.

  Example:
  ```clojure
  (time (exec (u/config) {:PostScriptPy \"ls-demo\"} {:_x [\"a\nb\" \"a\nb\nc\"]}))
  ;; => 
  ;; Elapsed time: 116.616146 msecs
  ;; {:ToExchange {:FileAmount [2 3]}}
  ```"
  [{conf :post} task data]
  (let [ps (pp-script conf task)]
    (if (u/file? ps)
      (let [res (sh "python3" ps (che/encode data) (che/encode task))]
        (if (= 0 (:exit res))
          (try
            (che/decode (:out res) true)
            (catch Exception e
              (µ/log ::exec :exception e :status :failed)
              {:error (str "caught exception: " (.getMessage e))}))
          {:error (:err res)}))
      {:error (str "no such file: " ps)})))


