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

;;------------------------------------------------------------
;; dispatch pre scripting or processing
;;------------------------------------------------------------
(defn pre-dispatch
  "Dispatches the pre-processing. The following processing paths are
  implemented:

  * `:PreProcessing`: eval javascript strings
  * `:PreScript`: clojure functions
  * `:PreScriptPy`: python scripts

  The pre-processing returns the **task**."
  [conf task]
  (if (:error task) task
      (cond
        (:PreScript     task) (pp/pre-dispatch conf task)
        (:PreProcessing task) (js/exec          conf task)
        (:PreScriptPy   task) (py/exec          conf task)
        :else (do (µ/log ::post-dispatch :req-id (:req-id task) :message "no pre-processing")
                  task))))

;;------------------------------------------------------------
;; dispatch post scripting or processing
;;------------------------------------------------------------
(defn post-dispatch
  "Dispatches the post-processing. The following processing paths are
  implemented:

  * `:PostScript`: clojure functions
  * `:PostProcessing`: javascript strings
  * `:PostScriptPy`: python scripts

  The pre-processing returns the **data**."
  [conf task]
  (if (:error task) task
      (cond
        (:PostScript     task) (pp/post-dispatch  conf task)
        (:PostProcessing task) (js/exec           conf task)
        (:PostScriptPy   task) (py/exec           conf task)
        :else (do
                (µ/log ::post-dispatch :req-id (:req-id task) :message "no post-processing")
                task))))

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
    (if (:error task)
      :error
      (do (µ/log ::dispatch :req-id (:req-id task) :Action (:Action task))
          (keyword (:Action task))))))
  
(defmethod dispatch :error   [conf task] task)
(defmethod dispatch :TCP     [conf task] (tcp/handler     conf task))
(defmethod dispatch :MODBUS  [conf task] (modbus/handler  conf task))
(defmethod dispatch :VXI11   [conf task] (vxi/handler     conf task))
(defmethod dispatch :EXECUTE [conf task] (execute/handler conf task))

(defmethod dispatch :default
  [conf task]
  (let [msg "wrong :Action"]
    (µ/log ::dispatch :req-id (:req-id task) :error msg :Action (:Action task))
    (merge task {:error msg})))

;;------------------------------------------------------------
;; request thread
;;------------------------------------------------------------
(defn thread 
  [conf task stub?]
  (let [task (safe/task conf task)]
    (let [task (pre-dispatch conf task)]
      (let [task (if stub? (stub/response conf task) (dispatch conf task))]
        (let [task (sample/record conf task)]
          (post-dispatch conf task))))))

;;------------------------------------------------------------
;; routes
;;------------------------------------------------------------
(defroutes app-routes
  (µ/trace ::routes
    [:function "routes"]
    (POST "/stub"   [:as req] (res/response (thread (u/config) (u/task req) true)))
    (POST "/"       [:as req] (res/response (thread (u/config) (u/task req) false)))
    (POST "/echo"   [:as req] (res/response (u/task req)))
    (GET "/version" [:as req] (res/response (u/version)))
    (route/not-found "No such service.")))

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
