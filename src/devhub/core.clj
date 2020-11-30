(ns devhub.core
  (:require
    [compojure.core       :refer :all]
    [compojure.route      :as route]
    [compojure.handler    :as handler]
    [devhub.stub          :as stub]
    [devhub.tcp           :as tcp]
    [devhub.conf          :as c]
    [devhub.utils         :as u]
    [aleph.http           :as aleph]
    [ring.util.response   :as res]
    [ring.middleware.json :as middleware]))

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

(defonce server (atom nil))

(defn start []
  (reset! server (aleph/start-server app {:port 9009})))

(defn stop []
  (when-not (nil? @server)
    (.close @server)
    (reset! server nil)))
