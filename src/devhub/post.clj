(ns devhub.post
  (:require [ring.util.response :as res]
            [clojure.string     :as string]
            [cheshire.core      :as che]
            [devhub.utils       :as u]
            [devhub.conf        :as c]))

(defn js-pp
  [pp data]
  (when (and (map? data) (vector? pp) (:_x data))
    (-> (string/join pp)
        (string/replace (re-pattern "_x")       (che/encode (:_x       data)))
        (string/replace (re-pattern "_t_start") (che/encode (:_t_start data)))
        (string/replace (re-pattern "_t_stop")  (che/encode (:_t_stop  data)))
        )))

(defn dispatch
  [conf {pp :PostProcessing ps :PostScript py :PyPostScript} {data :data error :error}]
  (if error
    error
    (cond
      pp (js-pp pp data)
      ;; ps (clj-pp ps data)
      ;; py (py-pp py data)
      )))
