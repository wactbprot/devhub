(ns devhub.stub
  ^{:author "Wact B. Prot <wactbprot@gmail.com>"
    :doc "Handles stub requests."}
  (:require [devhub.config :as c]
            [devhub.utils :as u]
            [devhub.safe :as safe]
            [com.brunobonacci.mulog :as mu]))

(defn select-response
  "Selects the response depending on the configuration. Implemented
  methods are:

  * `:rand` (default)
  * `:first` (fallback)
  * `:last`"
  [conf task]
  (let [resps (c/all-responses conf)
        resp  (or ((:select task) resps) (:missing resps))]
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
  [conf task]
  (mu/trace 
   ::response [:function "stub/response"]
   (if-not (:stub task) task
           (if (:error task) task
               (let [req-id (:req-id task)]
                 (if-let [task (safe/stub conf task)]
                   (let [f (fn [_] (select-response conf task))]
                     (mu/log ::response :message "call select-response"
                             :req-id req-id)
                     (merge task (if-let [data (u/run f conf task)]
                                   (u/reshape data)
                                   (let [msg "no data"]
                                     (mu/log ::response :error msg :req-id req-id)
                                     {:error "no data"}))))
                   (let [msg "can not derive keyword fron task name"]
                     (mu/log ::response :error msg :req-id req-id)
                     {:error msg})))))))

