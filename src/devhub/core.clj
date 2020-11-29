(ns devhub.core
  (:require
    [compojure.core :as compojure :refer [GET POST]]
    [compojure.route :as route]
    [aleph.http :as aleph]
    [byte-streams :as bs]
    [manifold.stream :as s]
    [manifold.deferred :as d]
    [ring.middleware.json   :as middleware]
])

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
    (.close @server)
    (reset! server nil)))

 

(defn start [] (reset! server (aleph/start-server handler {:port 10000})))
