(ns devhub.utils
  ^{:author "Wact B. Prot <wactbprot@gmail.com>"
    :doc "The devhub utils."}
  (:require [clojure.string         :as string]
            [clojure.pprint         :as pp]
            [clojure.edn            :as edn]
            [clojure.java.shell     :refer [sh]]
            [clojure.java.io        :as io]
            [com.brunobonacci.mulog :as mu])
  (:import  [java.net InetAddress]))

(defn connectable? [{host :Host}]
  (try (InetAddress/getByName host)
       true
       (catch Exception e false)))

(defn tmp-folder [] (System/getProperty "java.io.tmpdir"))

(defn print-body [req] (pp/pprint (:body req)))

(defn file? [f] (some? (io/resource f)))

(defn config
  "Reads a `edn` configuration in file `f`."
  ([]
   (config "conf.edn"))
  ([f]
   (-> (io/resource f) slurp edn/read-string)))

(defn responses-file [conf] (get-in conf [:stub :response-file]))

(defn all-responses  [conf] (config (responses-file conf)))

(defn stub-mode      [conf] (get-in conf [:stub :mode]))

(defn record-sample? [conf] (get-in conf [:sample :record]))

(defn version
  "Returns the latest `:version` and `:hash`."
  []
  {:version (:out (sh "git" "describe"))
   :hash (:out (sh "git" "rev-parse" "HEAD"))})

(defn map-vals
  "Map f over every value of m.  Returns a map with the same keys as m,
  where each of its values is now the result of applying f to them one
  by one.  f is a function of one arg, which will be called which each
  value of m, and should return the new value.  Faster then
  map-vals-transient on small maps (8 elements and under)"
  [f m]
  (reduce-kv (fn [m k v] (assoc m k (f v))) {} m))

(defn single-meas? [{t :_t_start}] (not (vector? t)))

(defn data [task] {:_x (:_x task) :_t_start (:_t_start task) :_t_stop (:_t_stop task)})

(defn reshape
  "Returns `data` if `data` is a map. Transforms `data` from a single
  measurement to a measurement vector if the length is greater than 1.

  REVIEW: There is a better solution (better than 4 times
  `mapv`). However, n=10 takes 0.217172 msecs

  Solution one (1.41838 msecs):
  ```clojure
  (reduce (fn [n o] (update-in
                     (update-in
                      (update-in
                       (update-in n [:_t_stop] conj (:_t_stop o))
                     [:_t_start] conj (:_t_start o))
                    [:_x] conj (:_x o))
                   [:_dt] conj (:_dt o)))
        {:_x [] :_t_start [] :_t_stop []}
        (flatten v))
  ```"
  [data]
  (cond
    (:error  data) data
    (vector? data) (let [v (flatten data)]
                     {:_x       (mapv :_x       v)
                      :_t_start (mapv :_t_start v)
                      :_t_stop  (mapv :_t_stop  v)}) 
    (map?    data) data
    (nil?    data) (let [msg "no data"]
                     (mu/log ::reshape :error msg)
                     {:error msg})))


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
    (string? x) (try
                  (Double/parseDouble x)
                  (catch Exception ex 
                    (mu/log ::number :error (.getMessage ex) :raw-result-str x))) 
    (number? x) x))

(defn ms [] (str (inst-ms (java.util.Date.))))

(defn task [req] (assoc (:body req) :req-id (ms)))
(defn action [req] (:Action (task req)))
(defn task-name [req] (:TaskName (task req)))

;;------------------------------------------------------------
;; run with wait and repeat
;;------------------------------------------------------------
(defn timestamp [m t0 t1] (assoc m :_t_start t0 :_t_stop  t1))

(defn wrap-log
  [{req-id :req-id} f]
  (fn [cmd]
    (let [raw-result (f cmd)]
      (mu/log ::wrap-log :req-id req-id :raw-result-str (str (:_x raw-result))
              :command cmd)
      raw-result)))

(defn wrap-times
  [f]
  (fn [cmd] (let [t0 (ms)] (timestamp {:_x (f cmd)} t0 (ms)))))

(defn run
  "Calls the function `f` with  all commands in `cmds` (vector of
  strings or int).`repeat`s (int) and `wait`s (int) in between if `(>
  repeat 1)`."
  [f conf {cmds :Value  w :Wait n :Repeat :as task}]
  (if (= 1 n (count cmds))
    ((wrap-log task (wrap-times f)) (first cmds))
    (mapv (fn [i]
            (let [v (mapv (wrap-log task (wrap-times f)) cmds)]
              (when (< i (dec n)) (Thread/sleep w))
              v))
          (range n))))

(defn ascii-logo
  []
  (println "                   __                           ")
  (println "                   \\ \\                          ")
  (println "                    \\ \\                         ")
  (println "                     > \\                        ")
  (println "                    / ^ \\                       ")
  (println "                   /_/ \\_\\                      ")
  (println "     _                  _               _       ")
  (println "  __| |   ___  __   __ | |__    _   _  | |__    ")
  (println " / _` |  / _ \\ \\ \\ / / | '_ \\  | | | | | '_ \\   ")
  (println "| (_| | |  __/  \\ V /  | | | | | |_| | | |_) |  ")
  (println " \\__,_|  \\___|   \\_/   |_| |_|  \\__,_| |_.__/   ")
  (println "                                                ")
  "")


