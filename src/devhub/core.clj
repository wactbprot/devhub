(ns devhub.core
  (:require [clojure.string :refer [lower-case]]
            [clojure.java.io :refer [as-file]]
            [ring.adapter.jetty :refer [run-jetty]]
            [ring.middleware.json :refer [wrap-json-response wrap-json-body]]
            [ring.util.response :as res]))

(defn file-name
  [s]
  (if (string? s)
    (str "resources/" (lower-case s) ".edn")))

(defn file?
  [f]
  (.exists (as-file f)))

(defn ms
  []
  (str (inst-ms (java.util.Date.))))

(defn by-name
  [req]
  (if-let [n (get-in req [:body :TaskName])]
    (let [f (file-name  n) ]
      (if (file? f) 
        (try
          {:data (read-string (slurp f))}
           (catch Exception e
             {:error (.getMessage e)}))
        {:msg (str "no edn for task name: " n)}))
    {:msg "body don't contain a taskname"}))

(defn by-action
  [req]
  (if-let [n (get-in req [:body :Action])]
    (let [f (file-name n)]
      (if (file? f)
        (try
          {:data (read-string (slurp f))}
           (catch Exception e
             {:error (.getMessage e)}))
        {:msg (str "no edn for action: " n)}))
    {:msg "body don't contain a action"}))

(defn handler
  [req]
  (let [t-start     (ms)
        m-name      (by-name req)
        m-action    (by-action req)
        msg-name    (:msg m-name)
        msg-action  (:msg m-action)
        data-name   (:data m-name)
        data-action (:data m-action)]
    (cond
      (:error m-name)     (res/bad-request m-name)
      (:error m-action)   (res/bad-request m-action)
      (and
       msg-name
       msg-action)        (res/not-found {:error (str msg-action
                                                     ", "
                                                     msg-name)})
      (map? data-name)    (res/response
                           (assoc data-name
                                 :t_start t-start
                                 :t_stop (ms)))
      (map? data-action)  (res/response
                           (assoc data-action
                                  :t_start t-start
                                  :t_stop (ms))))))

(def app
  (wrap-json-response
   (wrap-json-body handler {:keywords? true :bigdecimals? true})))

(def server (run-jetty #'app  {:port 8008 :join? false}))

(defn start
  []
  (.start server))

(defn stop
  []
  (.stop server))
