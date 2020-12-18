(ns devhub.post
  (:require [ring.util.response :as res]
            [clojure.string     :as string]
            [cheshire.core      :as che]
            [clojure.java.shell :refer [sh]]
            [devhub.js-pp       :as js]
            [devhub.utils       :as u]
            [devhub.conf        :as c]))

(defn dispatch
  [conf task data]
  (let [{pp :PostProcessing
         ps :PostScript
         py :PyPostScript} task]
    (cond
      pp (js/exec conf task pp data)
      ;; ps (clj-pp ps data)
      ;; py (py-pp py data)
      )))
