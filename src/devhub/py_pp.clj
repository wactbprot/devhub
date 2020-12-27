(ns devhub.py-pp
  ^{:author "wactbprot"
    :doc "Handles python post-processing."}
  (:require [clojure.string     :as string]
            [cheshire.core      :as che]
            [clojure.java.shell :refer [sh]]
            [devhub.utils       :as u]
            [com.brunobonacci.mulog :as µ]))

(defn pp-file [conf task] (str (:py-path conf)  "/" (:PostScriptPy task)  ".py"))

(defn exec
  "Executes a predefined *python3* script given with the `:PostScriptPy` key.

  Example:
  ```clojure
  (def pc (u/config))
  (def data {:_x [\"a\\nb\" \"a\\nb\\nc\"]})
  (time
    (exec pc {:PostScriptPy \"ls-demo\"} data))
  ;; =>
  ;; Elapsed time: 116.616146 msecs
  ;; {:ToExchange {:FileAmount [2 3]}}
  ```"
  [{conf :post} task data]
  (let [ps (pp-file conf task)]
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


