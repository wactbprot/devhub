(ns devhub.safe
  ^{:author "Wact B. Prot <wactbprot@gmail.com>"
    :doc "Ensure task values."}
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
(defn fnc    [conf x] (keyword x))
(defn addr   [conf x] (u/number x))
(defn quant  [conf x] (u/number x))

(defn safe-cmd [conf cmd] cmd)

(defmulti task (fn [conf task] (keyword (:Action task))))

(defmethod task :TCP
  [conf task]
  (let [{h :Host   p :Port v :Value w :Wait r :Repeat n :NoReply} task]
    (if (and h v p)
      (assoc task
             :Port    (port conf p)
             :Wait    (wait conf w)
             :Value   (value conf v)
             :Repeat  (rep conf r)
             :NoReply (norepl conf n))
      {:error "missing <:Host>, <:Value> or <:Port>"})))

(defmethod task :VXI11
  [conf task]
  (let [{h :Host   d :Device v :Value w :Wait r :Repeat n :NoReply} task
        m (u/parse-device-str d)]
    (if (and v h m)
      (assoc (merge task m)
             :Value   (value conf v)
             :Wait    (wait conf w)
             :Repeat  (rep conf r)
             :NoReply (norepl conf n))
      {:error "missing <:Host>, <:Value> or <:Device>"})))

(defmethod task :MODBUS
  [conf task]
  (let [{h :Host a :Address q :Quantity fc :FunctionCode
         w :Wait r :Repeat  v :Value    n :NoReply} task]
    (if (and h a fc q)
      (assoc task
             :FunctionCode (fnc conf fc)
             :Value        (value conf v)
             :Address      (addr conf a)
             :Quantity     (quant conf q)
             :Repeat       (rep conf r)
             :NoReply      (norepl conf n))
      (:error "missing <:Host>, <:Address>, <:FuctionCode> "))))

(defmethod task :EXECUTE
  [conf task]
  (let [{c :Cmd} task]
    (when c (assoc task :Cmd  (safe-cmd conf c)))))

(defn stub  
  [conf task]
  (let [{t :TaskName w :Wait r :Repeat v :Value} task]
    (assoc task
           :select (sel conf t) 
           :Value  (value conf v)
           :Wait   (wait conf w)
           :Repeat (rep conf r))))

