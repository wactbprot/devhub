(ns devhub.stub
  ^{:author "Thomas Bock <wactbprot@gmail.com>"
    :doc "Handles stub requests."}
  (:require [devhub.config :as c]
            [devhub.utils :as u]
            [devhub.safe :as safe]
            [com.brunobonacci.mulog :as µ]))

(defn select-response
  "Selects the response depending on the configuration. Implemented
  methods are:

  * `:rand` (default)
  * `:first` (fallback)
  * `:last`"
  [conf {:keys [select] :as task}]
  (let [resps (c/all-responses conf)
        resp  (or (select resps) (:missing resps))]
    (condp = (c/stub-mode conf)
      :first (first resp)
      :last  (last  resp)
      :rand  (nth   resp (rand-int (count resp)))
      (first resp))))

(defn response
  "Gets and returns a stub response if one is registered in `:stub-response-file`.

  Example:
  ```clojure
  (response (c/config) {:TaskName \"VS_SE3-get-valves-pos\"})
  ```"
  [conf {:keys [req-id stub error] :as task}]
  (µ/trace ::response [:function "stub/response"]
           (if-not stub task
             (if error task
                 (if-let [task (safe/stub conf task)]
                   (let [f (fn [_] (select-response conf task))]
                     (merge task (if-let [data (u/run f conf task)]
                                   (u/reshape data)
                                   {:error "no data produced"})))
                   {:error "can not derive keyword fron task name"})))))
