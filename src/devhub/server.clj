(ns devhub.server
  ^{:author "Thomas Bock <wactbprot@gmail.com>"
    :doc "Start and stop the devhub server. Routing and dispatching."}
  (:require [compojure.route :as route]
            [devhub.config :as c]
            [devhub.utils :as u]
            [clojure.pprint :as pprint]
            [devhub.pp :as pp]
            [devhub.pp-js :as js]
            [devhub.pp-py :as py]
            [devhub.tcp :as tcp]
            [devhub.udp :as udp]
            [devhub.stub :as stub]
            [devhub.safe :as safe]
            [devhub.vxi11 :as vxi]
            [devhub.modbus :as modbus]
            [devhub.execute :as execute]
            [ring.util.response :as res]
            [compojure.core :refer :all]
            [compojure.handler :as handler]
            [org.httpkit.server :refer [run-server]]
            [ring.middleware.json :as middleware]
            [com.brunobonacci.mulog :as µ])
  (:gen-class))

(def server (atom nil))
(def logger (atom nil))

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
  [conf {:keys [PreScript PreProcessing PreScriptPy error] :as task}]
  (µ/trace ::pre-dispatch [:function "server/pre-dispatch"]
           (cond
             error          task
             PreScript      (pp/pre-dispatch conf task)
             PreProcessing  (js/exec conf task)
             PreScriptPy    (py/exec conf task)
             :else           task)))
  

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
  [conf {:keys [PostScript PostProcessing PostScriptPy error] :as task}]
  (µ/trace ::post-dispatch [:function "server/post-dispatch"]
             (cond
               error          task
               PostScript     (pp/post-dispatch conf task)
               PostProcessing (js/exec conf task)
               PostScriptPy   (py/exec conf task)
               :else task)))

;;------------------------------------------------------------
;; dispatch on action
;;------------------------------------------------------------
(defmulti dispatch
  "Dispatches depending on the `:Action`. The following protocols paths are
  implemented:

  * `:TCP`
  * `:UDP`
  * `:MODBUS`
  * `:VXI11`
  * `:EXECUTE`"  
  (fn [conf {:keys [Action stub error] :as task}]
    (cond
      error :error
      stub :stub
      :else (keyword Action))))
  
(defmethod dispatch :error [conf task] task)
(defmethod dispatch :stub [conf task] task)
(defmethod dispatch :UDP [conf task] (udp/handler conf task))
(defmethod dispatch :TCP [conf task] (tcp/handler conf task))
(defmethod dispatch :MODBUS [conf task] (modbus/handler conf task))
(defmethod dispatch :VXI11 [conf task] (vxi/handler conf task))
(defmethod dispatch :EXECUTE [conf task] (execute/handler conf task))

(defmethod dispatch :default [conf {:keys [req-id Action] :as task}]
  (let [msg "wrong :Action"]
    (µ/log ::dispatch :req-id req-id :error msg :Action Action)
    (merge task {:error msg})))

;;------------------------------------------------------------
;; request go!
;;------------------------------------------------------------
(defn go! [conf task]
  (->> task
       (pre-dispatch conf)
       (safe/task conf)
       (stub/response conf)
       (dispatch conf)
       (post-dispatch conf)))

;;------------------------------------------------------------
;; routes
;;------------------------------------------------------------
(defroutes app-routes
  (POST "/stub"   [:as req] (res/response (go! (c/config) (assoc (u/task req) :stub true))))
  (POST "/"       [:as req] (res/response (go! (c/config) (u/task req))))
  (POST "/echo"   [:as req] (res/response (u/task req)))
  (GET "/version" [:as req] (res/response (u/version)))
  (route/not-found "No such service."))

(def app
  (-> (handler/site app-routes)
      (middleware/wrap-json-body {:keywords? true})
      (middleware/wrap-json-response)))

(defn init-log! [{conf :mulog ctx :log-context}]
  (µ/set-global-context! ctx)
  (µ/start-publisher! conf))

(defn stop []
  (µ/log ::stop)
  (@server :timeout 100)
  (reset! server nil)
  (@logger)
  (reset! logger nil))

(defn start
  ([] (start (c/config)))
  ([conf]
   (µ/log ::start)
   (reset! logger (init-log! conf))
   (reset! server (run-server #'app (:server conf)))))

(defn -main [& args]
  #_(pprint/pprint (c/config))
  (u/ascii-logo)
  (start))


