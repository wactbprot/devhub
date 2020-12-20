(ns devhub.execute
  (:require [clojure.java.shell :refer [sh]]
            [devhub.utils       :as u]))

(defn safe-cmd [conf cmd] cmd)

(defn safe
  "Ensures the `task` values to be in the right shape.
  TODO: safe-cmd"
  [conf task]
  (let [{cmd :Cmd w :Wait r :repeat} task]
    (when cmd 
      (assoc task
             :Repeat (if r (u/number r) (:repeat conf))
             :Wait   (if w (u/number w) (:min-wait conf))
             :Cmd    (if (string? cmd) [cmd] cmd)))))

(defn handler
  "Handles Execute tasks.
  
  Example:
  ```clojure
  (handler (u/config) {:Cmd \"ls\"})
  ```"
  [{conf :execute} task]
  (if-let [task (safe conf task)]
    (let [f (fn [cmd]
              (try
                (sh (:shell conf) (:param conf) cmd)
                (catch Exception e {:err (.getMessage e)})))]
      (if-let [data (u/run f (:Cmd task) (:Wait task) (:Repeat task))]
        (u/meas-vec data)
        {:error true :reason "no data"}))
    {:error true :reason "missing <value>, <host> or <port>"}))
