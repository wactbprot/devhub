(ns devhub.execute
  ^{:author "wactbprot"
    :doc "Handles EXECUTE Actions."}
  (:require [clojure.java.shell     :refer [sh]]
            [devhub.safe            :as safe]
            [devhub.utils           :as u]
            [com.brunobonacci.mulog :as µ]))

(defn safe-cmd [conf cmd] cmd)

(defn handler
  "Handles Execute tasks.
  
  Example:
  ```clojure
  (handler (u/config) {:Cmd \"ls\"})
  ```"
  [{conf :execute} task]
  (if-let [task (safe/execute conf task)]
    (let [f (fn [cmd] (:out (sh (:shell conf) (:param conf) cmd)))]
      (if-let [data (try (u/run f conf task)
                         (catch Exception e
                           (µ/log ::handler :exception (.getMessage e) :req-id (:req-id task))
                           {:error (.getMessage e)}))]
        (u/meas-vec data)
        {:error true :reason "no data"}))
    {:error true :reason "missing <value>, <host> or <port>"}))
