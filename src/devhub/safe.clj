(ns devhub.safe
  (:require [devhub.utils :as u]))

(defn value
  [conf v]
  (cond
    (nil? v)    [:no-value]
    (string? v) [v]
    (vector? v) v))

(defn wait   [conf w] (if w (u/number w) (:min-wait conf)))
(defn rep    [conf r] (if r (u/number r) (:repeat conf)))
(defn sel    [conf t] (if t (keyword t) :missing))
(defn port   [conf p] (u/number p))
(defn norepl [conf n] (if n n false))
(defn cmd    [conf c] (if (string? c) [c] c))

(defn stub
  "Ensures `task`s to be in the right shape."
  [conf task]
  (let [{t :TaskName w :Wait r :Repeat v :Value} task]
    (assoc task
           :select (sel conf t) 
           :Value  (value conf v)
           :Wait   (wait conf w)
           :Repeat (rep conf r))))

(defn tcp
  "Ensures tcp `task`s to be in the right shape."
  [conf task]
  (let [{h :Host p :Port v :Value w :Wait r :Repeat n :NoReply} task]
    (when (and h v p) (assoc task
                             :Port    (port conf p)
                             :Wait    (wait conf w)
                             :Value   (value conf v)
                             :Repeat  (rep conf r)
                             :NoReply (norepl conf n)))))

(defn vxi
  "Ensures vxi `task`s to be in the right shape."
  [conf task]
  (let [{h :Host d :Device v :Value w :Wait r :Repeat n :NoReply} task
        m (u/parse-device-str d)]
    (when (and v h m) (assoc (merge task m)
                             :Value   (value conf v)
                             :Wait    (wait conf w)
                             :Repeat  (rep conf r)
                             :NoReply (norepl conf n)))))

(defn execute
  "Ensures the `task` values to be in the right shape.
  TODO: safe-cmd"
  [conf task]
  (let [{c :Cmd w :Wait r :Repeat} task]
    (when c (assoc task
                   :Repeat (rep conf r)
                   :Wait   (wait conf w)
                   :Cmd    (cmd conf c)))))
