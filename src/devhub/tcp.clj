(ns devhub.tcp
  (:require
   [ring.util.response :as res]
   [devhub.utils       :as u]
   [devhub.conf       :as c]
   )
  (:import
   [java.io BufferedReader  OutputStreamWriter InputStreamReader PrintWriter]
   [java.net Socket]))

(defn send-receive
  "Sends the command `cmd` to the `out`put-stream. Receives data
  from the `in`put-stream. Wraps time stamps around."
  [in out cmd]
  (let [t0  (u/ms)]
    (.print out cmd)
    (.flush out)
    (let [res (.readLine in)
          t1  (u/ms)]
      (u/add-times {:_x res} t0 t1))))
  
(defn query
  "Sends an `cmd` a raw tcp socket with the specified `host` and
  `port`."
  [host port cmds wait repeat]
  (with-open [sock (Socket. host port)
              out (PrintWriter.    (OutputStreamWriter. (.getOutputStream sock)))
              in  (BufferedReader. (InputStreamReader.  (.getInputStream sock)))
              ]
    (mapv (fn [_]
            (let [v (mapv (fn [cmd]
                            (send-receive in out cmd))
                          cmds)]
              (Thread/sleep wait)
              v))
      (range repeat))))

(defn handler
  "Handles TCP queries.
  
  Example:
  ```clojure
  (handler (:tcp (c/config)
           {:Wait 10 :Repeat 3 :Port 5025  :Host \"e75496\"  :Value \"frs()\n\"})
  ;; =>
  
  ```"
  [tcp-conf {w :Wait r :Repeat p :Port h :Host v :Value }]
  (if (and v h p )
    (res/response (query h
                         (u/number p)
                         (if (string? v) [v] v)
                         (if w (u/number w) (:min-wait tcp-conf))
                         (if r (u/number r) (:repeat tcp-conf))))
    (res/response {:error true :reason "missing <value>, <host> or <port>"})))
  
