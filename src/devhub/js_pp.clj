(ns devhub.js-pp
  (:require [clojure.string     :as string]
            [cheshire.core      :as che]
            [clojure.java.shell :refer [sh]]
            [devhub.utils       :as u]
            [devhub.conf        :as c]))

(defn pp-str
  [pp data]
  (when (and (map? data) (vector? pp) (:_x data))
    (string/replace
     (-> (string/join pp)
         (string/replace (re-pattern "_x")       (che/encode (:_x       data)))
         (string/replace (re-pattern "_t_start") (che/encode (:_t_start data)))
         (string/replace (re-pattern "_t_stop")  (che/encode (:_t_stop  data))))
     #";" ";\n")))

(defn exec
  [{conf :post} task-name pp data]
  (let [path    (:js-path conf)
        exec    (str path "/" (:js-exec conf))
        pp-file (str (:js-tmp conf) "/" task-name ".js")]
    (spit pp-file (pp-str pp data))
    (sh "node" exec path pp-file)))
