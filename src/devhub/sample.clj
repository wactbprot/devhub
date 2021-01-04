(ns devhub.sample
  ^{:author "Wact B. Prot <wactbprot@gmail.com>"
    :doc "Records samples from outer space (or not)."}
  (:require [devhub.utils :as u]
            [devhub.safe :as safe]))

(defn record-sample?
  [conf]
  (if (:sample conf)
    (get-in conf [:sample :record])
    (get-in conf [:record])))

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

