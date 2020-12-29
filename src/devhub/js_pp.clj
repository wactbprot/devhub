(ns devhub.js-pp
  ^{:author "Wact B. Prot <wactbprot@gmail.com>"
    :doc "Handles javascript post-processing."}
  (:require [clojure.string         :as string]
            [cheshire.core          :as che]
            [clojure.java.shell     :refer [sh]]
            [devhub.utils           :as u]
            [com.brunobonacci.mulog :as µ]))

(defn pp-str
  [pp data]
  (when (and (map? data) (vector? pp) (:_x data))
     (-> (string/join pp)
         (string/replace (re-pattern "_x")       (che/encode (:_x       data)))
         (string/replace (re-pattern "_t_start") (che/encode (:_t_start data)))
         (string/replace (re-pattern "_t_stop")  (che/encode (:_t_stop  data))))))

(defn pp-fn [conf task] (str (:tmp conf)     "/" (:TaskName task) ".js"))
(defn exec-fn [conf]    (str (:js-path conf) "/" (:js-exec conf)))

(defn exec
  "Executes the js `:PostProcessing` (pp).

  Example:
  ```shell
  ;; (sh node (exec-fn conf) (:js-path conf) pf)
  ;; means e.g.:
  node resources/js/exec.js resources/js/ /tmp/MKT50-exec.js
  ```"
  [{conf :post} task data]
  (let [pf  (pp-fn conf task)
        _   (spit pf (pp-str (:PostProcessing task) data))
        res (sh (:js-interpreter conf) (exec-fn conf) (:js-path conf) pf)]
    (if (= 0 (:exit res))
      (try
        (che/decode (:out res) true)
        (catch Exception e
          (µ/log ::exec :error "decode error" :req-id (:req-id task))
          {:error (str "caught exception: " (.getMessage e))}))
      {:error (:err res)})))
