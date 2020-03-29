(ns devhub.core
  (:require [ring.adapter.jetty :refer [run-jetty]]
            [ring.middleware.json :refer [wrap-json-response wrap-json-body]]
            [ring.util.response :refer [response]]))

(defonce srv (atom nil))

(defn handler [req]
  (let [acc (get-in req [:body :Action])]
    (condp = (keyword acc)
      :MODBUS (response (read-string (slurp "resources/modbus.edn"))))))

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
