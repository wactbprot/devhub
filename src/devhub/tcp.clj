(ns devhub.tcp
  (:require

   [byte-streams       :as bs]
   [manifold.deferred  :as d]
   [ring.util.response :as res]
   [manifold.stream    :as s]
   [aleph.tcp          :as tcp]
   [devhub.utils       :as u]))

(defn handler
  [{w :Wait r :Repeat p :Port h :Host v :Value }]
  (if (and v h p )
    (let [c  @(tcp/client {:host h :port p})
          r  @(s/put! c v)]
      (prn r)
      (if r
        (let [b @(s/take! c)]
          (prn b)
          (prn (bs/to-string b))
          (res/response (res/status {:result (String. (byte-array b))} 200)))
          (res/response (res/status {:error true :reason "on attempt to put value"} 400))))
    (res/response (res/status {:error true :reason "no value, host or port given"} 400))))
