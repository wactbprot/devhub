(ns devhub.utils
  (:require [clojure.string :refer [lower-case]]
            [clojure.pprint :as pp]
            [clojure.java.io :refer [as-file]]))

(defn file-name [s] (when (string? s) (str "resources/" (lower-case s) ".edn")))

(defn file? [f] (.exists (as-file f)))

(defn ms [] (str (inst-ms (java.util.Date.))))

(defn task [req] (:body req))

(defn action [req] (:Action (task req)))

(defn task-name [req] (:TaskName (task req)))

(defn add-times [m t0 t1] (assoc m :t_start t0 :t_stop t1))

(defn content
  [f]
  (if (file? f) 
    (try
      {:data (read-string (slurp f))}
      (catch Exception e
        {:error (.getMessage e)}))
    {:msg (str "no edn resource for: " f)}))

(defn by-name
  [req]
  (if-let [n (task-name req)]
    (content (file-name n))
    {:msg "body don't contain a taskname"}))

(defn by-action
  [req]
  (if-let [n (action req)]
    (content (file-name  n))
    {:msg "body don't contain a action"}))

(defn print-body [req] (pp/pprint (:body req)))