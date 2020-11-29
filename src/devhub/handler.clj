(ns devhub.handler
  (:require [ring.util.response :as res]
            [devhub.utils :as u]))

(defn stub
  [conf req]
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

(defn mirror [conf req] (res/response (task req)))

(defn tcp-handler
  [{wait :Wait repeat :Repeat port :Port host :Host value :Value }])

(defn dispatch
  [conf req]
  (let [t0   (u/ms)] 
    (condp = (keyword (u/action req))
      :TCP  (tcp-handler (task req))
      (res/response {:error "not implemented"} )))) 