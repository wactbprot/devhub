(ns devhub.js-pp
  (:require [clojure.string     :as string]
            [cheshire.core      :as che]
            [clojure.java.shell :refer [sh]]
            [devhub.utils       :as u]
            [devhub.conf        :as c]))

(defn pp-str
  [pp data]
  (when (and (map? data) (vector? pp) (:_x data))
     (-> (string/join pp)
         (string/replace (re-pattern "_x")       (che/encode (:_x       data)))
         (string/replace (re-pattern "_t_start") (che/encode (:_t_start data)))
         (string/replace (re-pattern "_t_stop")  (che/encode (:_t_stop  data))))))

(defn pp-fn [conf task] (str (:js-tmp conf)  "/" (:TaskName task) ".js"))
(defn exec-fn [conf]    (str (:js-path conf) "/" (:js-exec conf)))

(defn exec
  "Executes the js `:PostProcessing` (pp).

  Example:
  ```shell
  ;; (sh node (exec-fn conf) (:js-path conf) pf)
  ;; means e.g.:
  node resources/js/exec.js resources/js/ /tmp/MKT50-exec.js
  ```
  "
  [{conf :post} task pp data]
  (let [pf  (pp-fn conf task)
        _   (spit pf (pp-str pp data))
        res (sh "node" (exec-fn conf) (:js-path conf) pf)]
    (if (:out res)
      (try (che/decode (:out res) true)
           (catch Exception e
             {:error (str "caught exception: " (.getMessage e))}))
      {:error (:err res)})))
