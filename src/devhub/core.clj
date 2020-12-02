(ns devhub.core
(:require [compojure.route        :as route]
          [devhub.conf            :as c]
          [devhub.utils           :as u]
          [devhub.handler         :as h]
          [clojure.edn            :as edn]
          [clojure.java.io        :as io]
          [clojure.tools.logging  :as log]
          [ring.util.response     :as res]
          [compojure.core         :refer :all]
          [compojure.handler      :as handler]
          [org.httpkit.server     :refer [run-server]]
          [ring.middleware.json   :as middleware]))

(defonce server (atom nil))

(defroutes app-routes
  (POST "/stub"   [:as req] (stub/handler (c/config) req))
  (POST "/echo"   [:as req] (res/response (u/task req)))
  (POST "/prod"   [:as req] (condp = (keyword (u/action req))
                              :TCP          (tcp/handler (u/task req))
                              (res/status {:error "not implemented"} 400)))
  (GET "/version" [:as req] (System/getProperty "devhub.version"))
  (route/not-found "No such service."))

(def app
  (-> (handler/site app-routes)
      (middleware/wrap-json-body {:keywords? true})
      middleware/wrap-json-response))

(defn stop
  []
  (when-not (nil? @server)
    (@server :timeout 100)
    (reset! server nil)))

(defn start [] (reset! server (run-server app {:port 9009})))
