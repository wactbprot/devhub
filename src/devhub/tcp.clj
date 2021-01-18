(ns devhub.tcp
  ^{:author "Wact B. Prot <wactbprot@gmail.com>"
    :doc "Handles TCP Actions."}
  (:require [devhub.utils           :as u]
            [com.brunobonacci.mulog :as mu])
  (:import [java.io BufferedReader OutputStreamWriter InputStreamReader PrintWriter]
           [java.net Socket]))

(defn out-socket [s] (PrintWriter. (OutputStreamWriter. (.getOutputStream s))))
(defn in-socket [s] (BufferedReader. (InputStreamReader. (.getInputStream s))))
(defn gen-socket [{h :Host p :Port}] (Socket. h p))

(defn query
  "Sends the `cmds` given by the `:Value` to a raw tcp socket with the
  specified `host` and `port`.

  Example:
  ```clojure
  (def c (u/config))
  (def t {:Port 5025 :Host \"e75496\" :Value [\"room()\\n\"] :Wait 10 :Repeat 2})
  (query c t)
  ```"
  [{conf :tcp} task]
  (if-not (u/connectable? task)
    {:error "can not connect"}
    (with-open [sock (gen-socket task)
                out  (out-socket sock)
                in   (in-socket sock)]
      (let [f (fn [cmd]
                (.print out cmd)
                (.flush out)
                (if-not (:NoReply task) (.readLine in) ""))]
        (u/run f conf task)))))

(defn handler
  "Handles TCP queries."
  [conf task]
  (mu/trace
   ::handler [:function "tcp/handler"]
   (if (:error task) task
       (let [{host :Host port :Port req-id :req-id} task
             _    (mu/log ::query :req-id req-id :Host host :Port port)
             data (query conf task)]
         (merge task (if (:error data)
                       (let [msg (:error data)]
                         (mu/log ::query :error msg :req-id req-id)
                         data)
                       (let [msg "received data"]
                         (mu/log ::query :message msg :req-id req-id)
                         (u/reshape data))))))))
  
