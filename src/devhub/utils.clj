(ns devhub.utils
  (:require [clojure.string  :refer [lower-case]]
            [clojure.pprint  :as pp]
            [clojure.edn     :as edn]
            [clojure.java.io :refer [as-file]]))

(defn config
  "Reads a `edn` configuration in file `f`."
  ([]
   (config "resources/conf.edn"))
  ([f]
   (-> f slurp edn/read-string)))

(defn version [] {:version (System/getProperty "devhub.version")})

(defn meas-vec
  "Returns `data` if `data` is a map. Transforms `data` from a single measurement to a measurement.

  REVIEW: There is a better solution (better than 4 times `mapv`). However, n=10 takes 0.217172 msecs

  solution one (1.41838 msecs):
  ```clojure
  (reduce (fn [n o] (update-in
                     (update-in
                      (update-in
                       (update-in n [:_t_stop] conj (:_t_stop o))
                     [:_t_start] conj (:_t_start o))
                    [:_x] conj (:_x o))
                   [:_dt] conj (:_dt o)))
        {:_x [] :_dt [] :_t_start [] :_t_stop []}
        (flatten v))
  ```"
  [data]
  (if (map? data) data (let [v (flatten data)]
                         {:_x       (mapv :_x       v)
                          :_t_start (mapv :_t_start v)
                          :_t_stop  (mapv :_t_stop  v)
                          :_dt      (mapv :_dt      v)})))

(defn number
  "Ensures the `x` to be a `number` or `nil`.

  ```clojure
  (number 1234)
  ;; =>
  ;; 1234
  (number true)
  ;; =>
  nil
  (number \"1234\")
  ;; =>
  1234
  ```"
  [x]
  (cond
    (string? x) (edn/read-string x)
    (number? x) x))

(defn ms [] (str (inst-ms (java.util.Date.))))

(defn task [req] (:body req))
(defn action [req] (:Action (task req)))
(defn task-name [req] (:TaskName (task req)))

(defn add-times
  [m t0 t1]
  (assoc m
         :_t_start t0
         :_t_stop t1
         :_dt (- (number t1) (number t0))))

(defn wrap-times
  [f]
  (fn [cmd]
    (let [t0 (ms)]
      (add-times {:_x (f cmd)} t0 (ms)))))
 
(defn run
  "Calls the function `f` with a all commands in `cmds` (vector of
  strings or int).`repeat`s (int) and `wait`s (int) in between if `(>
  repeat 1)`."
  [f cmds wait rep]
  (let [rep? (> rep 1)
        tf   (wrap-times f)]
    (mapv (fn [i] (let [v (mapv tf cmds)]
                    (when rep? (Thread/sleep wait))
                    v))
          (range rep))))

(defn print-body [req] (pp/pprint (:body req)))
