(ns devhub.execute
  (:require [clojure.java.shell :refer [sh]]
            [devhub.safe        :as safe]
            [devhub.utils       :as u]))

(defn safe-cmd [conf cmd] cmd)

(defn handler
  "Handles Execute tasks.
  
  Example:
  ```clojure
  (handler (u/config) {:Cmd \"ls\"})
  ```"
  [{conf :execute} task]
  (if-let [task (safe/execute conf task)]
    (let [f (fn [cmd]
              (try
                (:out (sh (:shell conf) (:param conf) cmd))
                (catch Exception e {:err (.getMessage e)})))]
      (if-let [data (u/run f conf task)]
        (u/meas-vec data)
        {:error true :reason "no data"}))
    {:error true :reason "missing <value>, <host> or <port>"}))
