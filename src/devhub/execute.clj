(ns devhub.execute
  ^{:author "Thomas Bock <wactbprot@gmail.com>"
    :doc "Handles EXECUTE Actions."}
  (:require [devhub.config :as c]
            [clojure.java.shell :refer [sh]]
            [devhub.safe :as safe]
            [devhub.utils :as u]
            [com.brunobonacci.mulog :as µ]))

(defn handler
  "Handles `EXECUTE` tasks.

  Example:
  ```clojure
  (handler (c/config) {:Cmd \"ls\"})
  ```"
  [{{shell :shell param :param :as conf} :execute} {cmd :Cmd req-id :req-id error :error :as task}]
  (µ/trace  ::handler [:function "execute/handler"]
            (if error
              task
              (merge task (if-let [task (safe/task conf task)]
                            (let [t0   (u/ms)
                                  res  (sh shell param cmd)
                                  t1   (u/ms)
                                  data (if-not (zero? (:exit res))
                                         (let [msg (:err res)]
                                           (µ/log ::handler :error msg :req-id req-id)
                                           {:error msg})
                                         {:_x (:out res) :_t_start t0 :_t_stop t1})]
                              data)
                            (let [msg "missing <command>"]
                              (µ/log ::handler :error msg :req-id req-id)
                              {:error msg}))))))

