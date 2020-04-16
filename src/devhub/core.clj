(ns devhub.core
  (:require [clojure.string :refer [lower-case]]
            [clojure.java.io :refer [as-file]]
            [ring.adapter.jetty :refer [run-jetty]]
            [ring.middleware.json :refer [wrap-json-response wrap-json-body]]
            [ring.util.response :as res]))

(defonce srv (atom nil))

(defn handler [req]
  (if-let [task-name (get-in req [:body :TaskName])]
    (let [file-name (str "resources/" (lower-case task-name) ".edn")]
      (if (.exist (as-file file-name))
        (res/response (read-string (slurp file-name)))
        (res/bad-request {:error (str "unknown task name: " task-name)})))
    (res/bad-request {:error "body don't contain a taskname"})))

(def app
  (wrap-json-response
   (wrap-json-body handler {:keywords? true :bigdecimals? true})))

(def server (run-jetty #'app  {:port 8008 :join? false}))

(defn start
  []
  (.start server))

(defn stop
  []
  (.stop server))
