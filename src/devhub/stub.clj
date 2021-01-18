(ns devhub.stub
  ^{:author "Wact B. Prot <wactbprot@gmail.com>"
    :doc "Handles stub requests."}
  (:require [devhub.utils :as u]
            [devhub.safe :as safe]
            [com.brunobonacci.mulog :as µ]))

(defn select-response
  "Selects the response depending on the configuration. Implemented
  methods are:

  * `:rand` (default)
  * `:first` (fallback)
  * `:last`"
  [kw rs mode]
  (let [r  (kw rs)]
    (condp = mode
      :first (first r)
      :last  (last  r)
      :rand  (nth   r (rand-int (count r)))
      (first r))))

(defn response
  "Gets and returns a stub response if one is registered in `:stub-response-file`.

  Example:
  ```clojure
  (response (u/config) {:TaskName \"VS_SE3-get-valves-pos\"})
  ```"
  [conf task]
  (if (:error task) task
      (if-let [task (safe/stub conf task)]
        (let [f (fn [_] (select-response (:select task) (u/all-responses conf) (u/stub-mode conf)))]
          (µ/log ::response :message "call select-response via u/run")
          (merge task (if-let [data (u/run f conf task)]
                        (u/reshape data)
                        (let [msg "no data"]
                          (µ/log ::response :error msg)
                          {:error "no data"}))))
        (let [msg "can not derive keyword fron task name"]
          (µ/log ::response :error msg)
          {:error msg}))))

