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
            [com.brunobonacci.mulog   :as µ])
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
            (µ/log ::post-dispatch :req-id (:req-id task)
                   :message "no pre-processing")
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
            (µ/log ::post-dispatch :req-id (:req-id task)
                   :message "no post-processing")
            data)))


;;------------------------------------------------------------
;; dispatch on action
;;------------------------------------------------------------
(defmulti dispatch 
  "Dispatches depending on the `:Action`. The following protocols paths are
  implemented:

  * `:TCP`
  * `:MODBUS`
  * `:VXI11`
  * `:EXECUTE`"  
  (fn [conf task]
    (µ/log ::dispatch :req-id (:req-id task) :Action (:Action task))
    (keyword (:Action task))))

(defmethod dispatch :TCP     [conf task] (tcp/query       conf task))
(defmethod dispatch :MODBUS  [conf task] (modbus/query    conf task))
(defmethod dispatch :VXI11   [conf task] (vxi/query       conf task))
(defmethod dispatch :EXECUTE [conf task] (execute/handler conf task))
(defmethod dispatch :default [conf task] {:error "wrong :Action"})

;;------------------------------------------------------------
;; request thread
;;------------------------------------------------------------
(defn error? [x] (:error x))
        
(defn thread 
  [conf task stub?]
  (µ/with-context {:req-id (:req-id task)}
    (let [task (µ/trace ::thread
                 [:function "safe/task"]
                 (safe/task conf task))]
      (if (error? task) task
          (let [task (µ/trace ::thread
                       [:function "pre-dispatch"]
                       (pre-dispatch conf task))]
            (if (error? task) task
                (let [data (if stub?
                             (µ/trace ::thread
                               [:function "stub/response"]
                               (stub/response conf task))
                             (µ/trace ::thread
                               [:function "dispatch"]
                               (dispatch conf task)))]
                  (if (error? data) data
                      (let [data (µ/trace ::thread
                                   [:function "u/meas-vec"]
                                   (u/meas-vec data))]
                        (if (error? data) data
                            (let [data (µ/trace ::thread
                                         [:function "sample/record"]
                                         (sample/record conf task data))]
                              (if (error? data) data
                                  (µ/trace ::thread
                                    [:function "post-dispatch"]
                                    (post-dispatch conf task data))))))))))))))

;;------------------------------------------------------------
;; routes
;;------------------------------------------------------------
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
  (µ/set-global-context!
   {:app-name "devhub" :version (:version (u/version))})
  (µ/start-publisher! conf))

(def server (atom nil))
(def logger (atom nil))

(defn stop
  []
  (µ/log ::stop)
  (@server :timeout 100)
  (reset! server nil)
  (@logger)
  (reset! logger nil))

(defn start
  ([]
   (start (u/config)))
  ([conf]
   (µ/log ::start)
   (reset! logger (init-log! conf))
   (reset! server (run-server #'app (:server conf)))))

(defn -main [& args] (u/ascii-logo)(start))
