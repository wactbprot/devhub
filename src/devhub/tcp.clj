(ns devhub.tcp
  ^{:author "Wact B. Prot <wactbprot@gmail.com>"
    :doc "Handles TCP Actions."}
  (:require [devhub.utils :as u]
            [devhub.safe  :as safe]
            [com.brunobonacci.mulog :as µ])
  (:import [java.io BufferedReader OutputStreamWriter InputStreamReader PrintWriter]
           [java.net Socket]))

(defn query
  "Handles TCP queries. Sends the `cmds` to a raw tcp socket with the specified `host` and
  `port`.
    
  Example:
  ```clojure
  (def c (u/config))
  (def t {:Port 5025 :Host \"e75496\" :Value [\"ch101()\\n\"] :Wait 10 :Repeat 2})
  (query c t)
  ;; =>
  ;; [[{:_x 0.0044590592716,0.0058012749458,5,ch101,
  ;;  :_t_start 1609747396222,
  ;;  :_t_stop 1609747400893}]
  ;; [{:_x -0.00040249268526,0.00023712185466,5,ch101,
  ;;  :_t_start 1609747400904,
  ;;  :_t_stop 1609747405575}]]
  ```"
  [{conf :tcp} task]
  (let [{host :Host port :Port} task]
    (µ/log ::query :req-id (:req-id task) :Host host :Port port)
    (try
      (with-open [sock (Socket. host port)
                  out  (PrintWriter.    (OutputStreamWriter. (.getOutputStream sock)))
                  in   (BufferedReader. (InputStreamReader. (.getInputStream sock)))]
        (u/run (fn [cmd]
                 (.print out cmd)
                 (.flush out)
                 (if-not (:NoReply task) (.readLine in) "")) conf task))
      (catch  Exception e
        (µ/log ::query :error "connection error"  :req-id (:req-id task))
        {:error "can not connect to host"}))))
