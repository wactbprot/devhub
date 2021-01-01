(ns devhub.server
  ^{:author "Wact B. Prot <wactbprot@gmail.com>"
    :doc "Start and stop the devhub server. Routing and dispatching."}
  (:require [compojure.route          :as route]
            [devhub.utils             :as u]
            [devhub.post-scripts.core :as clj]
            [devhub.js-pp             :as js]
            [devhub.py-pp             :as py]
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
            [com.brunobonacci.mulog   :as μ])
  (:gen-class))

(defn pre-dispatch
  [conf task]
  (let [{pp :PreProcessing
         ps :PreScript
         py :PreScriptPy} task]
    (μ/log ::pre-dispatch :req-id (:req-id task) :PreProcessing pp :PreScript ps :PreScriptPy py)
    (cond
      pp (js/exec conf task)
      ;; ps (clj-pp ps data)
      :else task)))

(defn post-dispatch
  [conf task data]
  (let [{pp :PostProcessing
           ps :PostScript
           py :PostScriptPy} task]
      (μ/log ::post-dispatch :req-id (:req-id task)  :PostProcessing pp :PostScript ps :PostScriptPy py)
      (cond
        pp (js/exec      conf task data)
        ps (clj/dispatch conf task data)
        py (py/exec conf task data)
        :else data)))
  
(defn dispatch
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
  (μ/log ::thread :req-id (:req-id task) :stub stub? :task-name (:TaskName task))
  (let [task (safe/task conf task)]
    (if (:error task) task
        (let [task (pre-dispatch conf task)]
          (if (:error task) task
              (let [data (if stub? (stub/response conf task) (dispatch conf task))]
                (if (:error data) data
                    (post-dispatch conf task (u/meas-vec data)))))))))

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


(defn -main [& args]
  (println "                   __                           ")   
  (println "                   \\ \\                          ")
  (println "                    \\ \\                         ")
  (println "                     > \\                        ")
  (println "                    / ^ \\                       ")
  (println "                   /_/ \\_\\                      ")
  (println "     _                  _               _       ")
  (println "  __| |   ___  __   __ | |__    _   _  | |__    ")
  (println " / _` |  / _ \\ \\ \\ / / | '_ \\  | | | | | '_ \\   ")
  (println "| (_| | |  __/  \\ V /  | | | | | |_| | | |_) |  ")
  (println " \\__,_|  \\___|   \\_/   |_| |_|  \\__,_| |_.__/   ")
  (println "                                                ")
  (start))
