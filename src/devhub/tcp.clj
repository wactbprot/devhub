(ns devhub.tcp
  (:require [devhub.utils       :as u]
            [devhub.conf        :as c])
  (:import [java.io BufferedReader OutputStreamWriter InputStreamReader PrintWriter]
           [java.net Socket]))

(defn send-receive
  "Sends the command `cmd` to the `out`put-stream. Receives data
  from the `in`put-stream. Wraps time stamps around."
  [in out norep cmd]
  (.print out cmd)
  (.flush out)
  (let [t0 (u/ms) res (if-not norep (.readLine in) "") t1 (u/ms)]
    (u/add-times {:_x res} t0 t1)))

(defn query
  "Sends the `cmds` to a raw tcp socket with the specified `host` and
  `port`."
  [conf task]
  (let [{host :Host port :Port cmds :Value wait :Wait repeat :Repeat norep :NoReply} task]
    (with-open [sock (Socket. host port)
                out (PrintWriter.    (OutputStreamWriter. (.getOutputStream sock)))
                in  (BufferedReader. (InputStreamReader. (.getInputStream sock)))]
      (let [pf  (partial (fn [cmd] (send-receive in out norep cmd)))]
        (u/run pf cmds wait repeat)))))

(defn safe
  "Ensures the `task` values to be in the right shape."
  [conf task]
  (let [{h :Host p :Port v :Value w :Wait r :Repeat n :NoReply} task]
    (when (and h v p) (assoc task
                             :Port    (u/number p)
                             :Wait    (if w (u/number w) (:min-wait conf))
                             :Value   (if (string? v) [v] v)
                             :Repeat  (if r (u/number r) (:repeat conf))
                             :NoReply (if n n false)))))

(defn handler
  "Handles TCP queries.
  
  Example:
  ```clojure
  (handler (:tcp (c/config))
           {:Wait 10 :Repeat 3 :Port 5025 :Host \"e75496\" :Value \"frs()\n\"})
  ```"
  [{conf :tcp} task]
  (if-let [task (safe conf task)]
    (if-let [data (query conf task)]
      {:data (u/meas-vec data)}
      {:error true :reason "no data"})
    {:error true :reason "missing <value>, <host> or <port>"}))
