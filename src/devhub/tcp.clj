(ns devhub.tcp
  (:require [devhub.utils       :as u]
            [devhub.conf        :as c])
  (:import [java.io BufferedReader OutputStreamWriter InputStreamReader PrintWriter]
           [java.net Socket]))

(defn send-receive
  "Sends the command `cmd` to the `out`put-stream. Receives data
  from the `in`put-stream. Wraps time stamps around."
  [in out cmd]
  (.print out cmd)
  (.flush out)
  (let [t0  (u/ms)
        res (.readLine in)
        t1  (u/ms)]
    (u/add-times {:_x res} t0 t1)))

(defn query
  "Sends a `cmd` a raw tcp socket with the specified `host` and
  `port`. `repeat`s and `wait`s in between."
  [host port cmds wait repeat]
  (with-open [sock (Socket. host port)
              out (PrintWriter.    (OutputStreamWriter. (.getOutputStream sock)))
              in  (BufferedReader. (InputStreamReader. (.getInputStream sock)))]
    (mapv (fn [_]
            (let [v (mapv (fn [cmd] (send-receive in out cmd)) cmds)]
              (Thread/sleep wait)
              v))
      (range repeat))))

(defn handler
  "Handles TCP queries.
  
  Example:
  ```clojure
  (handler (:tcp (c/config))
           {:Wait 10 :Repeat 3 :Port 5025 :Host \"e75496\" :Value \"frs()\n\"}) 

  ```"
  [tcp-conf {w :Wait r :Repeat p :Port h :Host v :Value}]
  (if (and v h p )
    (if-let [data (query h (u/number p) (if (string? v) [v] v)
                         (if w (u/number w) (:min-wait tcp-conf))
                         (if r (u/number r) (:repeat tcp-conf)))]
      {:data (u/meas-vec data)}
      {:error true :reason "no data"})
    {:error true :reason "missing <value>, <host> or <port>"}))
