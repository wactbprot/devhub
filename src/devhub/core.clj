(ns devhub.core
  (:require [compojure.route        :as route]
            [devhub.utils           :as u]
            [devhub.post-scripts.core :as clj]
            [devhub.js-pp           :as js]
            [devhub.tcp             :as tcp]
            [devhub.stub            :as stub]
            [devhub.vxi11           :as vxi]
            [devhub.modbus          :as modbus]
            [devhub.execute         :as execute]
            [ring.util.response     :as res]
            [compojure.core         :refer :all]
            [compojure.handler      :as handler]
            [org.httpkit.server     :refer [run-server]]
            [ring.middleware.json   :as middleware]
            [com.brunobonacci.mulog :as μ]))

;;(μ/start-publisher! {:type :console :pretty? true})
(μ/start-publisher!
  {:type :elasticsearch
   :url  "http://localhost:9200/"})

(defn pre-dispatch
  [conf task]
  (μ/with-context {:fn (meta #'pre-dispatch)}
  (let [{pp :PreProcessing
         ps :PreScript
         py :PreScriptPy} task]
    (μ/log ::call :PreProcessing pp :PreScript ps :PreScriptPy py)
    (cond
      pp (js/exec conf task)
      ;; ps (clj-pp ps data)
      :else task))))

(defn post-dispatch
  [conf task data]
  (μ/with-context {:fn (meta #'post-dispatch)}
    (let [{pp :PostProcessing
           ps :PostScript
           py :PostScriptPy} task]
      (μ/log ::call :PostProcessing pp :PostScript ps :PostScriptPy py)
      (prn data)
      (cond
        pp (js/exec conf task pp data)
        ps (clj/dispatch conf task data)
        :else data))))

(defn dispatch
  [conf task]
  (μ/with-context {:fn (meta #'dispatch)}
    (let [action (keyword (:Action task))]
      (μ/log ::call :Action action)
      (condp = action 
        :TCP     (tcp/handler     conf task)
        :MODBUS  (modbus/handler  conf task)
        :VXI11   (vxi/handler     conf task)
        :EXECUTE (execute/handler conf task)
        {:error "wrong action"}))))

(defn thread
  [conf task stub?]
  (μ/with-context {:fn (meta #'thread)}
    (μ/log ::call :stub stub? :task-name (:TaskName task))
    (let [task (pre-dispatch conf task)]
      (if (:error task)
        task
        (let [data (if stub?
                     (stub/response conf task)
                     (dispatch      conf task))]
          (if (:error data)
            data
            (post-dispatch conf task data)))))))

(defroutes app-routes
  (POST "/stub"   [:as req] (res/response (thread (u/config) (u/task req) true)))
  (POST "/"       [:as req] (res/response (thread (u/config) (u/task req) false)))
  (POST "/echo"   [:as req] (res/response (u/task req)))
  (GET "/version" [:as req] (res/response (u/version)))
  (route/not-found "No such service."))

(def app
  (-> (handler/site app-routes)
      (middleware/wrap-json-body {:keywords? true})
      middleware/wrap-json-response))

(defonce server (atom nil))
(defn stop  [] (@server :timeout 100) (reset! server nil))
(defn start [] (reset! server (run-server app (:server (u/config)))))
