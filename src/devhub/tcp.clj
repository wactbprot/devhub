(ns devhub.tcp
  ^{:author "Wact B. Prot <wactbprot@gmail.com>"
    :doc "Handles TCP Actions."}
  (:require [devhub.utils           :as u]
            [com.brunobonacci.mulog :as µ])
  (:import [java.io BufferedReader OutputStreamWriter InputStreamReader PrintWriter]
           [java.net Socket]))

(defn query
  "Handles TCP queries. Sends the `cmds` to a raw tcp socket with the
  specified `host` and `port`.
    
  Example:
  ```clojure
  (def c (u/config))
  (def t {:Port 5025 :Host \"e75496\" :Value [\"ch101()\\n\"] :Wait 10 :Repeat 2})
  (query c t)
  ```"
  [{conf :tcp} task]
  (let [{host :Host port :Port} task
        data (try
               (µ/log ::query :req-id (:req-id task) :Host host :Port port)
               (with-open [sock (Socket. host port)
                           out  (PrintWriter.    (OutputStreamWriter. (.getOutputStream sock)))
                           in   (BufferedReader. (InputStreamReader. (.getInputStream sock)))]
                 (u/run (fn [cmd]
                          (.print out cmd)
                          (.flush out)
                          (if-not (:NoReply task) (.readLine in) "")) conf task))
               (catch  Exception e
                 (let [msg "connection error, can not connect to host"]
                   (µ/log ::query :error msg :req-id (:req-id task))
                   {:error msg})))]
    (merge task (u/reshape data))))
