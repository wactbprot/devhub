(ns devhub.pp-js
  ^{:author "Wact B. Prot <wactbprot@gmail.com>"
    :doc "Handles javascript post-processing."}
  (:require [clojure.string         :as string]
            [cheshire.core          :as che]
            [clojure.java.shell     :refer [sh]]
            [devhub.utils           :as u]
            [clojure.java.io        :as io]            
            [com.brunobonacci.mulog :as µ]))

(defn exec-file [conf]  (str (:js-path conf) "/" (:js-exec conf)))

(defn gen-pp-source [task] (string/join (:PostProcessing task))) 

(defn gen-pp-data [task] (che/encode (u/data task)))

  
(defn exec
  "Executes the js `:PostProcessing` (pp).

  Example:
  ```shell
  ;; (sh node (exec-fn conf) (:js-path conf) pf)
  ;; means e.g.:
  node resources/js/exec.js resources/js/ /tmp/MKT50-exec-source.js /tmp/MKT50-exec-data.js
  ```"
 [{conf :post} task]
  (let [req-id (:req-id task)
        pp     (gen-pp-source task) data (gen-pp-data task)
        res    (sh (:js conf) (exec-file conf) (:js-path conf) pp data)]
    (µ/log ::exec :message "exec pp-js" :pp-data data :pp-source pp :req-id req-id)
    (if-not (zero? (:exit res))
      (let [msg (:err res)]
         (µ/log ::exec :error msg :req-id req-id)
         (merge task {:error msg}))
      (merge task (try
                    (che/decode (:out res) true)
                    (catch Exception e
                      (let [msg (str "caught exception: " (.getMessage e))]
                        (µ/log ::exec :error msg :req-id req-id)
                        {:error msg})))))))
