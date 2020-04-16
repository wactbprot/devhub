(ns devhub.core
  (:require [devhub.utils :as u]
            [ring.adapter.jetty :refer [run-jetty]]
            [ring.middleware.json :refer [wrap-json-response
                                          wrap-json-body]]
            [ring.util.response :as res]))

(defn handler
  [req]
  (let [t0          (u/ms)
        m-name      (u/by-name req)
        m-action    (u/by-action req)
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
                           (u/add-times data-name t0 (u/ms)))
      (map? data-action)  (res/response
                           (u/add-times data-action t0 (u/ms))))))
  
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
