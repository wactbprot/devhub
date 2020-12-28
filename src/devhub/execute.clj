(ns devhub.execute
  ^{:author "wactbprot"
    :doc "Handles EXECUTE Actions."}
  (:require [clojure.java.shell     :refer [sh]]
            [devhub.safe            :as safe]
            [devhub.utils           :as u]
            [com.brunobonacci.mulog :as µ]))

(defn handler
  "Handles Execute tasks.
  
  Example:
  ```clojure
  (handler (u/config) {:Cmd \"ls\"})
  ```"
  [{conf :execute} task]
  (if-let [task (safe/execute conf task)]
    (let [t0  (u/ms)
          res (sh (:shell conf) (:param conf) (:Cmd task))
          t1  (u/ms)]
      (if (= 0 (:exit res))
        {:_x (:out res) :_t_start t0 :_t_stop t1} 
        {:error (:err res)}))
    {:error "missing <command>"}))
