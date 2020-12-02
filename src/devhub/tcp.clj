(ns devhub.tcp
  (:require
   [clojure.java.io    :as io]
   [ring.util.response :as res]
   [devhub.utils       :as u])
  (:import
   [java.io StringWriter]
   [java.net Socket]))

(defn send-request
  "Sends an HTTP GET request to the specified host, port, and path"
  [host port value]
  (let [t0 (u/ms)]
    (with-open [sock (Socket. host port)
                writer (io/writer sock)
                reader (io/reader sock)
                response (StringWriter.)]
      (.append writer value)
      (.flush writer)
      (io/copy reader response)
      (u/add-times {:_x (str response)} t0 (u/ms)))))

(defn handler
  "
  Example:
  ```clojure
  (handler {:Port 5025  :Host \"e75496\"  :Value \"frs()\n\"})
  ;; =>
  
  ```"
  [{w :Wait r :Repeat p :Port h :Host v :Value }]
  (if (and v h p )
    (res/response (send-request h p v))
    (res/response {:error true :reason "no <value>, <host> or <port> given"})))
  
