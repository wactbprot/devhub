(ns devhub.stub
  ^{:author "Wact B. Prot <wactbprot@gmail.com>"
    :doc "Handles stub requests."}
  (:require [devhub.utils :as u]
            [devhub.safe :as safe]))

(defn select-response
  "Selects the response depending on the configuration. Implemented
  methods are:

  * `:rand` (default)
  * `:first` (fallback)
  * `:last`"
  [kw rs mode]
  (let [r  (kw rs)]
    (condp = mode
      :first (first r)
      :last  (last  r)
      :rand  (nth   r (rand-int (count r)))
      (first r))))

(defn responses-file
  [conf]
  (if (:stub conf)
    (get-in conf [:stub :response-file])
    (get-in conf [:response-file])))

(defn record-sample?
  [conf]
  (if (:sample conf)
    (get-in conf [:sample :record])
    (get-in conf [:record])))

(defn all-responses [conf] (u/config (responses-file conf)))

(defn insert
  [{conf :sample} m kw {x :_x}]
  (let [n (:count conf)]
    (assoc m kw (if (kw m)
                  (into [] (take n (if (vector? x)
                                     (concat (take n x) (kw m))
                                     (conj (reverse (kw m)) x))))
                  (if (vector? x) (into [] (take n x)) [x])))))

(defn record
  [conf {task-name :TaskName} data]
  (when (and record-sample? task-name (:_x data))
    (spit (responses-file conf)
          (prn-str (insert conf (all-responses conf) (keyword task-name) data))))
  data)

(defn response
  "Gets and returns a stub response if one is registered in `:stub-response-file`.

  Example:
  ```clojure
  (response (u/config) {:TaskName \"VS_SE3-get-valves-pos\"})
  ```"
  [{conf :stub} task]
  (if-let [task (safe/stub conf task)]
    (let [f (fn [_] (select-response (:select task) (all-responses conf) (:mode conf)))]
      (if-let [data (u/run f conf task)]
        data
        {:error true :reason "no data"}))
    {:error "can not derive keyword fron task name"}))
