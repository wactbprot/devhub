(ns devhub.stub
  (:require [devhub.utils :as u]))

(defn select-response
  [kw rs mode]
  (let [r  (kw rs)]
    (condp = mode
      :first (first r)
      :last  (last  r)
      :rand  (nth   r (rand-int (count r)))
      (first r))))

(defn all-responses [conf] (u/config (:response-file conf))) 

(defn query-fn
  "Returns a function `f` that can be used in `(u/run f cmds wait rep)`"
  [conf task]
  (fn [_]
    (let [t0 (u/ms)]
      (u/add-times
       {:_x (select-response (:select task) (all-responses conf) (:mode conf))}
       t0 (u/ms)))))
    
(defn safe
  [conf task]
  (let [{t :TaskName w :Wait r :Repeat v :Value} task]
    (assoc task
           :select (if t (keyword t) :missing) 
           :Value  (cond
                     (nil? v)    [:no-value]
                     (string? v) [v]
                     (vector? v) v)
           :Wait   (if w (u/number w) (:min-wait conf))
           :Repeat (if r (u/number r) (:repeat conf)))))

(defn response
  "Gets and returns a stub response if one is registered in `:stub-response-file`.

  Example:
  ```clojure
  (response (u/config) {:TaskName \"VS_SE3-get-valves-pos\"})
  ```"
  [{conf :stub} task]
  (if-let [task (safe conf task)]
    (if-let [data (u/run (query-fn conf task) (:Value task) (:Wait task) (:Repeat task))]
      {:data (u/meas-vec data)}
      {:error true :reason "no data"})
    {:error "can not derive keyword fron task name"}))