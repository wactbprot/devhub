(ns devhub.tcp
  (:require [devhub.utils :as u]
            [devhub.safe  :as safe]
            [com.brunobonacci.mulog :as µ])
  (:import [java.io BufferedReader OutputStreamWriter InputStreamReader PrintWriter]
           [java.net Socket]))

(defn query
  "Sends the `cmds` to a raw tcp socket with the specified `host` and
  `port`."
  [conf task]
  (let [{host :Host port :Port norep :NoReply} task]
    (with-open [sock (Socket. host port)
                out  (PrintWriter.    (OutputStreamWriter. (.getOutputStream sock)))
                in   (BufferedReader. (InputStreamReader. (.getInputStream sock)))]
      (u/run (fn [cmd]
               (.print out cmd)
               (.flush out)
               (if-not norep (.readLine in) "")) conf task))))
            

(defn handler
  "Handles TCP queries.
    
  Example:
  ```clojure
  (def c (u/config))
  ;;
  (def t1 {:Port 5025 :Host \"e75496\" :Value \"frs()\\n\"}
  (handler c t1)
  ;;
  (def t2 {:Port 5000 :Host \"localhost\" :Value \"frs()\\n\"}
  (handler c t2)
  ```"
  [{conf :tcp} task]
  (if-let [task (safe/tcp conf task)]
    (if-let [data (try         
                    (query conf task)
                    (catch Exception e
                      (µ/log ::exec :exception e :status :failed :req-id (:req-id task))
                      {:error (str "caught exception: " (.getMessage e))}))]
      (u/meas-vec data)
      {:error true :reason "no data"})
    {:error true :reason "missing <value>, <host> or <port>"}))
