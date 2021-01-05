(ns devhub.sample
  ^{:author "Wact B. Prot <wactbprot@gmail.com>"
    :doc "Records samples from outer space (or not)."}
  (:require [devhub.utils    :as u]
            [clojure.java.io :as io]
            [com.brunobonacci.mulog :as µ]))

(defn insert
  [{conf :sample} m kw data]
  (let [n (:count conf) x (:_x data) t (:_t_start data)]
    (assoc m kw (if (kw m)
                  (into [] (take n (if (vector? t)
                                     (concat (take n x) (kw m))
                                     (conj (reverse (kw m)) x))))
                  (if (vector? t) (into [] (take n x)) [x])))))

(defn record
  [conf {task-name :TaskName} data]
  (when (and (u/record-sample? conf) task-name (:_x data))
    (µ/log ::record :message "record sample data" :TaskName task-name)
    (spit (io/resource (u/responses-file conf))
          (prn-str (insert conf (u/all-responses conf) (keyword task-name) data))))
  data)
