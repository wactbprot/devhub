(ns devhub.tcp
  (:require
   [byte-streams       :as bs]
   [manifold.deferred  :as d]
   [ring.util.response :as res]
   [manifold.stream    :as s]
   [aleph.tcp          :as tcp]
   [devhub.utils       :as u]))

(defn open-client [h p] @(tcp/client {:host h :port p}))

(defn close-client [c] (.close c))

(defn query
  [c v]
  (let [t0 (u/ms)
        r  @(s/put! c v)
        b  @(s/take! c)
        t1 (u/ms)]
    (u/add-times {:_x (bs/to-string b)} t0 t1)))

(defn handler
  "
  Example:
  ```clojure
  (handler {:Port 5025  :Host \"e75496\"  :Value \"frs()\n\"})
  ;; =>
  
  ```"
  [{w :Wait r :Repeat p :Port h :Host v :Value }]
  (if (and v h p )
    (let [c (open-client h p)
          m (query c v)]
      (close-client c)
      (res/response m))
    (res/response {:error true :reason "no <value>, <host> or <port> given"})))
  
