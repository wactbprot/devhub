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

(defn run
  "Executes the `partial` function `pf` with the commands
  `cmd`.`repeat`s and `wait`s in between."
  [pf cmds wait rep]
  (mapv (fn [i]
          (let [data-vec (mapv pf cmds)]
            (Thread/sleep wait)
            data-vec))
        (range rep)))

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

(defn file-name [s] (when (string? s) (str "resources/" (lower-case s) ".edn")))

(defn file? [f] (.exists (as-file f)))

(defn ms [] (str (inst-ms (java.util.Date.))))

(defn task [req]
  (prn req)
  (:body req))

(defn action [req] (:Action (task req)))

(defn task-name [req] (:TaskName (task req)))

(defn add-times
  [m t0 t1]
  (assoc m
         :_t_start t0
         :_t_stop t1
         :_dt (- (number t1) (number t0))))

(defn file-content
  [f]
  (if (file? f) 
    (try
      {:data (read-string (slurp f))}
      (catch Exception e
        {:error (.getMessage e)}))
    {:msg (str "no edn resource for: " f)}))

(defn by-name
  [req]
  (if-let [n (task-name req)]
    (file-content (file-name n))
    {:msg "body don't contain a taskname"}))

(defn by-action
  [req]
  (if-let [n (action req)]
    (file-content (file-name  n))
    {:msg "body don't contain a action"}))

(defn print-body [req] (pp/pprint (:body req)))
