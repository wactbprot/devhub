(ns devhub.server
  ^{:author "Wact B. Prot <wactbprot@gmail.com>"
    :doc "Start and stop the devhub server. Routing and dispatching."}
  (:require [compojure.route          :as route]
            [devhub.utils             :as u]
            [devhub.pp                :as pp]
            [devhub.pp-js             :as js]
            [devhub.pp-py             :as py]
            [devhub.tcp               :as tcp]
            [devhub.stub              :as stub]
            [devhub.safe              :as safe]
            [devhub.sample            :as sample]
            [devhub.vxi11             :as vxi]
            [devhub.modbus            :as modbus]
            [devhub.execute           :as execute]
            [ring.util.response       :as res]
            [compojure.core           :refer :all]
            [compojure.handler        :as handler]
            [org.httpkit.server       :refer [run-server]]
            [ring.middleware.json     :as middleware]
            [com.brunobonacci.mulog   :as μ])
  (:gen-class))

(defn pre-dispatch
  "Dispatches the pre-processing. The following processing paths are
  implemented:

  * `:PreProcessing`: eval javascript strings
  * `:PreScript`: clojure functions
  * `:PreScriptPy`: python scripts

  The pre-processing returns the **task**."
  [conf task]
  (cond
    (:PreScript     task) (pp/pre-dispatch conf task)
    (:PreProcessing task) (js/exec          conf task)
    (:PreScriptPy   task) (py/exec          conf task)
    :else (do
            (μ/log ::post-dispatch :req-id (:req-id task) :message "no pre-processing")
            task)))

(defn post-dispatch
  "Dispatches the post-processing. The following processing paths are
  implemented:

  * `:PostScript`: clojure functions
  * `:PostProcessing`: javascript strings
  * `:PostScriptPy`: python scripts

  The pre-processing returns the **data**."
  [conf task data]
  (cond
    (:PostScript     task) (pp/post-dispatch  conf task data)
    (:PostProcessing task) (js/exec           conf task data)
    (:PostScriptPy   task) (py/exec           conf task data)
    :else (do
            (μ/log ::post-dispatch :req-id (:req-id task) :message "no post-processing")
            data)))

(defn dispatch
  "Dispatches depending on the `:Action`. The following protocols paths are
  implemented:

  * `:TCP`
  * `:MODBUS`
  * `:VXI11`
  * `:EXECUTE`"
  [conf task]
    (let [action (keyword (:Action task))]
      (μ/log ::dispatch :req-id (:req-id task) :Action action)
      (condp = action
        :TCP     (tcp/query       conf task)
        :MODBUS  (modbus/query    conf task)
        :VXI11   (vxi/query       conf task)
        :EXECUTE (execute/handler conf task)
        {:error "wrong action"})))

(defn thread
  [conf task stub?]
  (μ/log ::thread :req-id (:req-id task) :stub stub? :TaskName (:TaskName task))
  (let [task (safe/task conf task)]
    (if (:error task) task
        (let [task (pre-dispatch conf task)]
          (if (:error task) task
              (let [data (if stub? (stub/response conf task) (dispatch conf task))]
                (if (:error data) data
                    (let [data (sample/record conf task (u/meas-vec data))]
                      (if (:error data) data
                          (let [data (post-dispatch conf task data)]
                            (if (:error data) data
                                (do
                                  (μ/log ::thread :req-id (:req-id task) :message "request complete")
                                  data))))))))))))

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

(defn init-log!
  [{conf :mulog }]
  (μ/set-global-context!
   {:app-name "devhub" :version (:version (u/version)) :env "local"})
  (μ/start-publisher! conf))

(def server (atom nil))
(def logger (atom nil))

(defn stop
  []
  (μ/log ::stop)
  (@server :timeout 100)
  (reset! server nil)
  (@logger)
  (reset! logger nil))

(defn start
  ([]
   (start (u/config)))
  ([conf]
   (μ/log ::start)
   (reset! logger (init-log! conf))
   (reset! server (run-server #'app (:server conf)))))

(defn -main [& args] (u/ascii-logo)(start))
