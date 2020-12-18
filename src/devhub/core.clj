(ns devhub.core
  (:require [compojure.route        :as route]
            [devhub.conf            :as c]
            [devhub.utils           :as u]
            [devhub.post            :as post]
            [devhub.tcp             :as tcp]
            [devhub.vxi11           :as vxi]
            [devhub.modbus          :as modbus]
            [devhub.execute         :as execute]
            [devhub.stub            :as stub]
            [ring.util.response     :as res]
            [compojure.core         :refer :all]
            [compojure.handler      :as handler]
            [org.httpkit.server     :refer [run-server]]
            [ring.middleware.json   :as middleware]))

(defn dispatch
  [conf task]
  (let [action (keyword (:Action task))
        data   (condp = action 
                 :TCP     (tcp/handler    conf task)
                 :MODBUS  (modbus/handler conf task)
                 :VXI11   (vxi/handler    conf task)
                 :EXECUTE (execute/handler conf task)
                 {:error "not implemented"})]
    (if (:error data)
      (res/response data)
      (post/dispatch conf task data)))) 

(defroutes app-routes
  (POST "/stub"   [:as req] (stub/handler (c/config) req))
  (POST "/echo"   [:as req] (res/response (u/task req)))
  (POST "/prod"   [:as req] (dispatch (c/config) (u/task req)))
  (GET "/version" [:as req] (res/response (u/version)))
  (route/not-found "No such service."))

(def app
  (-> (handler/site app-routes)
      (middleware/wrap-json-body {:keywords? true})
      middleware/wrap-json-response))

(defonce server (atom nil))

(defn stop
  []
  (when-not (nil? @server)
    (@server :timeout 100)
    (reset! server nil)))

(defn start [] (reset! server (run-server app (:server (c/config)))))
