(ns devhub.tcp
  (:require
   [ring.util.response :as res]
   [manifold.deferred  :as d]
   [manifold.stream    :as s]
   [aleph.tcp          :as tcp]
   [devhub.utils       :as u]))

(defn handler
  [{w :Wait r :Repeat p :Port h :Host v :Value }]
  (if (and v h p )
    (let [c  @(tcp/client {:host h :port p})
          r  @(s/put! c v)]
      (if r
        (res/status {:result (String. (byte-array @(s/take! c)))} 200)
        (res/status {:error true :reason "on attempt to put value"} 400)))
    (res/status {:error true :reason "no value, host or port given"} 400)))
