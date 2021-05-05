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
            [devhub.vxi11             :as vxi]
            [devhub.modbus            :as modbus]
            [devhub.execute           :as execute]
            [ring.util.response       :as res]
            [compojure.core           :refer :all]
            [compojure.handler        :as handler]
            [org.httpkit.server       :refer [run-server]]
            [ring.middleware.json     :as middleware]
            [com.brunobonacci.mulog   :as mu])
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
  (mu/trace
      ::pre-dispatch [:function "server/pre-dispatch"]
      (if (:error task) task
          (cond
            (:PreScript     task) (pp/pre-dispatch conf task)
            (:PreProcessing task) (js/exec          conf task)
            (:PreScriptPy   task) (py/exec          conf task)
            :else (do (mu/log ::pre-dispatch :req-id (:req-id task)
                              :message "no pre-processing")
                      task)))))
  
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
  (mu/trace
      ::post-dispatch [:function "server/post-dispatch"]
      (if (:error task) task
          (cond
            (:PostScript     task) (pp/post-dispatch  conf task)
            (:PostProcessing task) (js/exec           conf task)
            (:PostScriptPy   task) (py/exec           conf task)
            :else (do
                    (mu/log ::post-dispatch :req-id (:req-id task)
                            :message "no post-processing")
                    task)))))
  
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
      (do (mu/log ::dispatch :req-id (:req-id task) :Action (:Action task))
          (if (:stub task) :stub (keyword (:Action task)))))))
  
(defmethod dispatch :error   [conf task] task)
(defmethod dispatch :stub    [conf task] task)
(defmethod dispatch :TCP     [conf task] (tcp/handler     conf task))
(defmethod dispatch :MODBUS  [conf task] (modbus/handler  conf task))
(defmethod dispatch :VXI11   [conf task] (vxi/handler     conf task))
(defmethod dispatch :EXECUTE [conf task] (execute/handler conf task))

(defmethod dispatch :default
  [conf task]
  (let [msg "wrong :Action"]
    (mu/log ::dispatch :req-id (:req-id task) :error msg :Action (:Action task))
    (merge task {:error msg})))

;;------------------------------------------------------------
;; request thread
;;------------------------------------------------------------
(defn thread 
  [conf task]
  (->> task
       (pre-dispatch  conf)
       (safe/task     conf)
       (stub/response conf)
       (dispatch      conf)
       (post-dispatch conf)))
  
;;------------------------------------------------------------
;; routes
;;------------------------------------------------------------
(defroutes app-routes
  (POST "/stub"   [:as req] (res/response
                             (thread (u/config) (assoc (u/task req)
                                                       :stub true))))
  (POST "/"       [:as req] (res/response
                             (thread (u/config) (assoc (u/task req)
                                                       :stub false))))
  (POST "/echo"   [:as req] (res/response (u/task req)))
  (GET "/version" [:as req] (res/response (u/version)))
  (route/not-found "No such service."))

(def app
  (-> (handler/site app-routes)
      (middleware/wrap-json-body {:keywords? true})
      middleware/wrap-json-response))

(defn init-log!
  [{conf :mulog }]
  (mu/set-global-context!
   {:app-name "devhub" :version (:version (u/version))})
  (mu/start-publisher! conf))

(def server (atom nil))
(def logger (atom nil))

(defn stop
  []
  (mu/log ::stop)
  (@server :timeout 100)
  (reset! server nil)
  (@logger)
  (reset! logger nil))

(defn start
  ([]
   (start (u/config)))
  ([conf]
   (mu/log ::start)
   (reset! logger (init-log! conf))
   (reset! server (run-server #'app (:server conf)))))

(defn -main [& args] (u/ascii-logo)(start))
