(ns devhub.tcp
  (:require
   [ring.util.response :as res]
   [manifold.deferred  :as d]
   [manifold.stream    :as s]
   [clojure.edn        :as edn]
   [aleph.tcp          :as tcp]
   [devhub.utils       :as u]))


(defn handler
  [{wait :Wait repeat :Repeat port :Port host :Host value :Value }])
