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
  (POST "/stub"   [:as req] (h/stub     (c/config) req))
  (POST "/mirror" [:as req] (h/mirror     (c/config) req))
  (POST "/prod"   [:as req] (h/dispatch (c/config) req))
  (GET "/version" [:as req] (System/getProperty "devhub.version")))

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
