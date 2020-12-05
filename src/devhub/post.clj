(ns devhub.post
  (:require [ring.util.response :as res]
            [clojure.string     :as string]
            [cheshire.core      :as che]
            [clojure.java.shell :refer [sh]]
            [devhub.js-pp       :as js]
            [devhub.utils       :as u]
            [devhub.conf        :as c]))

(defn dispatch
  [conf {task-name :TaskName pp :PostProcessing ps :PostScript py :PyPostScript} {data :data error :error}]
  (if error
    error
    (cond
      pp (js/exec conf task-name pp data)
      ;; ps (clj-pp ps data)
      ;; py (py-pp py data)
      )))
