(ns devhub.config
  ^{:author "Wact B. Prot <wactbprot@gmail.com>"
    :doc "Handles the config. Cares about environment variables."}
  (:require [clojure.string  :as string]
            [clojure.edn     :as edn]
            [clojure.java.io :as io]))

(defn get-conf
  "Reads a `edn` configuration in file `f`."
  ([]
   (get-conf "conf.edn"))
  ([f]
   (-> (io/resource f) slurp edn/read-string)))

(defn log-type [conf]
  (let [e (keyword (System/getenv "DEVHUB_LOG_TYPE"))
        d (get-in conf [:mulog :elasticsearch])]
    (assoc-in conf
              [:mulog :publishers]
              (if (= e :off) [] [(get-in conf [:mulog e] d)]))))

(defn log-context [ conf]
  (if-let [s (System/getenv "DEVHUB_FACILITY")] 
    (assoc-in conf [:log-context :facility] s)
    conf))
  
(defn ip [conf]
  (if-let [e (System/getenv "DEVHUB_IP")]
    (assoc-in conf [:server :ip] e)
    conf))

(defn port [conf]
  (if-let [e (System/getenv "DEVHUB_PORT")]
    (assoc-in conf [:server :port] e)
    conf))

(defn responses-file [conf] (get-in conf [:stub :response-file]))
(defn all-responses [conf] (get-conf (responses-file conf)))
(defn stub-mode [conf] (get-in conf [:stub :mode]))
(defn config [] (-> (get-conf) log-type log-context ip port ))
