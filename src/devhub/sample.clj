(ns devhub.sample
  ^{:author "Wact B. Prot <wactbprot@gmail.com>"
    :doc "Records samples from outer space (or not)."}
  (:require [devhub.utils    :as u]
            [clojure.java.io :as io]
            [clojure.pprint  :as pp]
            [com.brunobonacci.mulog :as µ]))

(defn insert
  [{conf :sample} m kw task]
  (let [n (:count conf)
        x (:_x task)
        single-meas? (u/single-meas? task)]
    (assoc m kw (if (kw m)
                  (into [] (take n (if single-meas?
                                     (conj (reverse (kw m)) x)
                                     (concat (take n x) (kw m)))))
                  (if single-meas? [x] (into [] (take n x)))))))

(defn record
  "FIXME: record to database not to filesystem"
  [conf task]
  (let [{task-name :TaskName data :_x} task]
    (when (and (u/record-sample? conf) task-name data)
      (µ/log ::record :message "record sample data" :TaskName task-name)
      (spit (io/resource (u/responses-file conf))
            (with-out-str (pp/pprint (insert conf (u/all-responses conf) (keyword task-name) task)))))
    task))
