(ns devhub.tcp
  ^{:author "Wact B. Prot <wactbprot@gmail.com>"
    :doc "Handles TCP Actions."}
  (:require [devhub.utils :as u]
            [devhub.safe  :as safe]
            [com.brunobonacci.mulog :as µ])
  (:import [java.io BufferedReader OutputStreamWriter InputStreamReader PrintWriter]
           [java.net Socket]))

(defn query
  "Sends the `cmds` to a raw tcp socket with the specified `host` and
  `port`."
  [conf task]
  (let [{host :Host port :Port} task]
    (try
      (with-open [sock (Socket. host port)
                  out  (PrintWriter.    (OutputStreamWriter. (.getOutputStream sock)))
                  in   (BufferedReader. (InputStreamReader. (.getInputStream sock)))]
        (u/run (fn [cmd]
                 (.print out cmd)
                 (.flush out)
                 (if-not (:NoReply task) (.readLine in) "")) conf task))
      (catch  Exception e
      {:error "can not connect to host"}))))

(defn handler
  "Handles TCP queries.
    
  Example:
  ```clojure
  (def c (u/config))
  ;;
  (def t1 {:Port 5025 :Host \"e75496\" :Value \"frs()\\n\"})
  (handler c t1)
  ;;
  (def t2 {:Port 5000 :Host \"localhost\" :Value \"frs()\\n\"})
  (handler c t2)
  ```"
  [{conf :tcp} task]
    (let [data-or-err (query conf task)]
      (if (:error data-or-err)
        (let [error   data-or-err
              err-msg (:error error)]
          (µ/log ::handler :error err-msg :req-id (:req-id task))
          error)
        (let [data (u/meas-vec data-or-err)]
          (µ/log ::handler :data data  :req-id (:req-id task))
          data))))
