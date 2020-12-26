(ns devhub.utils
  (:require [clojure.string         :as string]
            [clojure.pprint         :as pp]
            [clojure.edn            :as edn]
            [clojure.java.io        :refer [as-file]]
            [com.brunobonacci.mulog :as µ]))

(defn file? [f] (.exists (as-file f)))

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
  (if (map? data)
      data
      (let [v (flatten data)]
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

(defn task [req] (assoc (:body req) :req-id (ms)))
(defn action [req] (:Action (task req)))
(defn task-name [req] (:TaskName (task req)))

;;------------------------------------------------------------
;; run with wait and repeat
;;------------------------------------------------------------
(defn add-times
  [m t0 t1]
  (assoc m
         :_t_start t0
         :_t_stop  t1
         :_dt (- (number t1) (number t0))))

(defn wrap-try
  [f]
  (fn [cmd]
    (try
      (f cmd)
      (catch Exception e
        (µ/log ::exec :exception e :status :failed)
        {:error (str "caught exception: " (.getMessage e))}))))

(defn wrap-log
  [req-id f]
  (fn [cmd]
    (let [raw-result (f cmd)]
     (µ/log ::wrap-log :req-id req-id :raw-result-str (str raw-result))
     raw-result)))

(defn wrap-times
  [f]
  (fn [cmd]
    (let [t0 (ms)]
      (add-times {:_x (f cmd)} t0 (ms)))))
 
(defn run
  "Calls the function `f` with  all commands in `cmds` (vector of
  strings or int).`repeat`s (int) and `wait`s (int) in between if `(>
  repeat 1)`."
  [f conf {cmds :Value wait :Wait rep :Repeat req-id :req-id}]
  (mapv (fn [_]
          (let [v (mapv (wrap-try (wrap-log req-id (wrap-times f))) cmds)]
            (when (> rep 1) (Thread/sleep wait))
            v))
        (range rep)))

(defn print-body [req] (pp/pprint (:body req)))

;;------------------------------------------------------------
;; vxi utils
;;------------------------------------------------------------
(defn parse-gpib-str
  "Returns a map with `:DeviceName` `:PrimaryAddress` and `:SecondaryAddress`.
  
  Example:
  ```clojure
  (parse-gpib-str \"gpib0,9\")
  ;; =>
  ;; {:DeviceName \"gpib0\", :PrimaryAddress 9, :SecondaryAddress 0}
 
  (parse-gpib-str \"gpib0,9,1\")
  ;; =>
  ;; {:DeviceName \"gpib0\", :PrimaryAddress 9, :SecondaryAddress 1}
  ``` "
  [s]
  (when-let [v (re-find (re-matcher #"(gpib[0-9]*),([0-9]*),?([0-9]*)" s))]
    {:DeviceName       (nth v 1)
     :PrimaryAddress   (number (nth v 2))
     :SecondaryAddress (if (= "" (nth v 3)) 0 (number (nth v 3)))}))

(defn parse-inst-str [s] {:DeviceName s :PrimaryAddress -1 :SecondaryAddress -1})
      
(defn parse-device-str
  [s]
  (when (string? s)
    (cond
      (string/starts-with? s "inst") (parse-inst-str s)
      (string/starts-with? s "gpib") (parse-gpib-str s)
      :else (throw "not a device string"))))

(defn bb->string
  "Turns the byte buffer `b` into a string.

  Example:
  ```clojure
  (time (bb->string (byte-array 1024)))
  ;; =>
  ;; Elapsed time: 0.347679 msecs
  ;; \"\"
  ```"
  [b]
  (string/replace (slurp b) #" " ""))
