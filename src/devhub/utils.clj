(ns devhub.utils
  (:require [clojure.string :refer [lower-case]]
            [clojure.java.io :refer [as-file]]))

(defn file-name
  [s]
  (if (string? s)
    (str "resources/" (lower-case s) ".edn")))

(defn file?
  [f]
  (.exists (as-file f)))

(defn ms
  []
  (str (inst-ms (java.util.Date.))))

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
  (if-let [n (get-in req [:body :TaskName])]
    (content (file-name  n))
    {:msg "body don't contain a taskname"}))

(defn by-action
  [req]
  (if-let [n (get-in req [:body :Action])]
    (content (file-name  n))
    {:msg "body don't contain a action"}))

(defn add-times
  [m t0 t1]
  (assoc m
         :t_start t0
         :t_stop t1))