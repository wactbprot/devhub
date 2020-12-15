(ns devhub.execute
  (:require [devhub.conf        :as c]
            [clojure.java.shell :refer [sh]]
            [devhub.utils       :as u]))

(defn execute
  "Executes the `cmd`.

  Example:
  ```clojure
  (execute (:execute (c/config)) {:Cmd \"ls\"})
  ```
  "
  [{s :shell p :param} {cmd :Cmd}]
  (let [t0 (u/ms)
        res (try (sh s p cmd) (catch Exception e {:err (.getMessage e)}))
        t1 (u/ms)]
    (if (:out res)
      (u/add-times {:_x (:out res)} t0 t1)
      {:error (:err res)})))

(defn safe-cmd [conf cmd] cmd)

(defn safe
  "Ensures the `task` values to be in the right shape."
  [conf task]
  (when-let [cmd (safe-cmd conf (:Cmd task))] task))

(defn handler
  "Handles Execute tasks.
  
  Example:
  ```clojure
  (handler (c/config) {:Cmd \"ls\"})
  ```"
  [{conf :execute} task]
  (if-let [task (safe conf task)]
    (if-let [data (execute conf task)]
      {:data (u/meas-vec data)}
      {:error true :reason "no data"})
    {:error true :reason "missing <value>, <host> or <port>"}))
