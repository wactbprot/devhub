(ns devhub.core
  (:require [compojure.route        :as route]
            [devhub.utils           :as u]
            [devhub.post-scripts.core :as clj]
            [devhub.js-pp           :as js]
            [devhub.tcp             :as tcp]
            [devhub.vxi11           :as vxi]
            [devhub.modbus          :as modbus]
            [devhub.execute         :as execute]
            [ring.util.response     :as res]
            [compojure.core         :refer :all]
            [compojure.handler      :as handler]
            [org.httpkit.server     :refer [run-server]]
            [ring.middleware.json   :as middleware]))


(defn post-dispatch
  [conf task data]
  (let [{pp :PostProcessing
         ps :PostScript
         py :PostScriptPy} task]
    (cond
      pp (js/exec conf task pp data)
      ps (clj/dispatch conf task data)
      :else data)))

(defn pre-dispatch
  [conf task]
  (let [{pp :PreProcessing
         ps :PreScript
         py :PostScriptPy} task
        task (cond
               pp (js/exec conf task)
               ;; ps (clj-pp ps data)
               :else task)]))

(defn dispatch
  [conf task]
  (let [task   (pre-dispatch conf task)
        action (keyword (:Action task))
        data   (condp = action 
                 :TCP     (tcp/handler     conf task)
                 :MODBUS  (modbus/handler  conf task)
                 :VXI11   (vxi/handler     conf task)
                 :EXECUTE (execute/handler conf task)
                 {:error "wrong action"})
        data    (post-dispatch conf task data)]
    (if (:error data)
      (res/response data)
      (post-dispatch conf task data)))) 

(defroutes app-routes
  (GET "/version" [:as req] (res/response (u/version)))
  (POST "/echo"   [:as req] (res/response (u/task req)))
  (POST "/stub"   [:as req] (dispatch
                             (assoc-in (u/config) [:stub :on] true)
                             (u/task req)))
  (POST "/"       [:as req] (dispatch
                             (u/config)
                             (u/task req)))
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

(defn start [] (reset! server (run-server app (:server (u/config)))))
