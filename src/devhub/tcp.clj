(ns devhub.tcp
  (:require
   [ring.util.response :as res]
   [devhub.utils       :as u])
  (:import
   [java.io DataInputStream DataOutputStream]
   [java.net Socket]))

(defn send-request
  "Sends an `cmd` a raw tcp socket with the specified `host` and
  `port`."
  [host port cmd]
  (let [t0 (u/ms)]
    (with-open [sock (Socket. host port)
                out  (DataOutputStream. (.getOutputStream sock))
                in   (DataInputStream. (.getInputStream sock))]
      (.writeUTF out cmd)
      (u/add-times {:_x (.readLine in)} t0 (u/ms)))))

(defn handler
  "Handles TCP requests.
  
  Example:
  ```clojure
  (handler {:Port 5025  :Host \"e75496\"  :Value \"frs()\n\"})
  ;; =>
  
  ```"
  [{w :Wait r :Repeat p :Port h :Host v :Value }]
  (if (and v h p )
    (res/response (send-request h p v))
    (res/response {:error true :reason "no <value>, <host> or <port> given"})))
  
