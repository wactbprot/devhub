(ns devhub.pp-py
  ^{:author "Wact B. Prot <wactbprot@gmail.com>"
    :doc "Handles python post-processing."}
  (:require [cheshire.core          :as che]
            [clojure.java.shell     :refer [sh]]
            [devhub.utils           :as u]
            [com.brunobonacci.mulog :as µ]))

(defn pp-file [conf task] (str (:PostScriptPy task)  ".py"))

(defn exec
  "Executes a predefined *python3* script given with the `:PostScriptPy` key.

  Example:
  ```clojure
  (def pc (u/config))
  (def data {:_x \"a\\nba\\nb\\ncb\"})
  (time
    (exec pc {:PostScriptPy \"ls-demo\"} data))
  ;; =>
  ;; Elapsed time: 31.680355 msecs
  ;; {:ToExchange {:FilesVector [a ba b cb]}}
  ```"
  [{conf :post} task]
  (let [ps (pp-file conf task)]
    (if-not (u/file? ps)
      (let [msg (str "no such postscript file: " ps)]
        (µ/log ::exec :error msg :req-id (:req-id task))
        (merge task {:error msg}))
      (let [res (sh (:py conf) ps (che/encode task) (che/encode (u/data task)) :dir (:py-path conf))]
        (merge task (if-not (zero? (:exit res))
                      {:error (:err res)}
                      (try
                        (che/decode (:out res) true)
                        (catch Exception e
                          (let [msg (str "decode error, caught exception: " (.getMessage e))]
                            (µ/log ::exec :error msg :req-id (:req-id task))
                            {:error msg})))))))))


