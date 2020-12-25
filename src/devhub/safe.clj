(ns devhub.safe
  (:require [devhub.utils           :as u]
            [com.brunobonacci.mulog :as μ]))

(defn value
  "Function ensures that a `vector` is returned." 
  [conf x]
  (cond
    (nil?    x) [:no-value]
    (string? x) [x]
    (vector? x)  x))

(defn wait   [conf x] (if x (u/number x) (:min-wait conf)))
(defn rep    [conf x] (if x (u/number x) (:repeat conf)))
(defn sel    [conf x] (if x (keyword x) :missing))
(defn port   [conf x] (u/number x))
(defn norepl [conf x] (if x x false))
(defn cmd    [conf x] (value conf x))
(defn fnc    [conf x] (keyword x))
(defn addr   [conf x] (u/number x))
(defn quant  [conf x] (u/number x))

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
  (let [{h :Host   p :Port v :Value w :Wait
         r :Repeat n :NoReply} task]
    (when (and h v p) (assoc task
                             :Port    (port conf p)
                             :Wait    (wait conf w)
                             :Value   (value conf v)
                             :Repeat  (rep conf r)
                             :NoReply (norepl conf n)))))

(defn vxi
  "Ensures vxi `task`s to be in the right shape."
  [conf task]
  (let [{h :Host   d :Device v :Value w :Wait
         r :Repeat n :NoReply} task
        m (u/parse-device-str d)]
    (when (and v h m) (assoc (merge task m)
                             :Value   (value conf v)
                             :Wait    (wait conf w)
                             :Repeat  (rep conf r)
                             :NoReply (norepl conf n)))))

(defn modbus
  [conf task]
  (let [{h :Host a :Address q :Quantity fc :FunctionCode
         w :Wait r :Repeat  v :Value    n :NoReply} task]
    (when (and h a fc q) (assoc task
                                :FunctionCode (fnc conf fc)
                                :Value        (value conf v)
                                :Address      (addr conf a)
                                :Quantity     (quant conf q)
                                :Repeat       (rep conf r)
                                :NoReply      (norepl conf n)))))

(defn execute
  "Ensures the `task` values to be in the right shape.
  TODO: safe-cmd"
  [conf task]
  (let [{c :Cmd w :Wait r :Repeat} task]
    (when c (assoc task
                   :Repeat (rep conf r)
                   :Wait   (wait conf w)
                   :Value  (cmd conf c)))))
