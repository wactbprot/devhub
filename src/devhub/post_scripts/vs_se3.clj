(ns devhub.post-scripts.vs_se3
  (:require [clojure.edn  :as edn]
            [devhub.utils :as u]
            [devhub.conf  :as c]))

(defn config [] (-> "resources/vs_se3.edn" slurp edn/read-string))

(defn open? [x n] (bit-test x n))

(defn input-ok?
  [x]
  (and (vector? x)
       (= (:all-block-count (config))
          (count x))))

(defn check
  [x v]
  (when (input-ok? x)
    (mapv
     (fn [kw [block position]] {kw (open? (nth x block) position)})
     v)))

(defn valves_state    [{x :_x}] (check x (:valve-position (config))))
(defn switches_open   [{x :_x}] (check x (:switch-open    (config))))
(defn switches_closed [{x :_x}] (check x (:switch-closed  (config))))


;; Exchange looks like this:
;;{
;;   "V1":{
;;      "Bool":1
;;         }, ...
;; 
;; "E1_open":{
;;      "Bool":true
;;   },
;;   "E1_closed":{
;;      "Bool":false
;;   }
;; }
