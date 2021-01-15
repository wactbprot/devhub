(ns devhub.safe
  ^{:author "Wact B. Prot <wactbprot@gmail.com>"
    :doc "Ensure task values."}
  (:require [clojure.string         :as string]
            [devhub.utils           :as u]
            [com.brunobonacci.mulog :as μ]))

(defn value
  "Function ensures that a `vector` is returned. Fallback value is
  `[:no-value]`."
  [conf x]
  (cond
    (string? x) [x]
    (vector? x)  x
    :else [:no-value]))

(defn wait   [conf x] (if x (u/number x) (:min-wait conf)))

(defn rep    [conf x] (if x (u/number x) (:repeat conf)))

(defn sel    [conf x] (if x (keyword x) :missing))

(defn port   [conf x] (u/number x))

(defn fnc    [conf x] (keyword x))

(defn addr   [conf x] (u/number x))

(defn quant  [conf x] (u/number x))

(defn safe-cmd [conf cmd] cmd)

(defn norepl
  "Reacts on legacy `VxiTimeout:0`."
  ([conf x]
   (or x false))
  ([conf x t]
   (or (= 0 t) x false))) 

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
     :PrimaryAddress   (u/number (nth v 2))
     :SecondaryAddress (if (= "" (nth v 3)) 0 (u/number (nth v 3)))}))

(defn parse-inst-str [s] {:DeviceName s :PrimaryAddress -1 :SecondaryAddress -1})

(defn parse-device-str
  [s]
  (when (string? s)
    (cond
      (string/starts-with? s "inst") (parse-inst-str s)
      (string/starts-with? s "gpib") (parse-gpib-str s)
      :else {:error "not a device string"})))

(defmulti task (fn [conf task] (keyword (:Action task))))

(defmethod task :default [conf task] task)

(defmethod task :TCP
  [{conf :tcp} task]
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
  [{conf :vxi} task]
  (let [{h :Host d :Device v :Value w :Wait r :Repeat n :NoReply t :VxiTimeout} task]
    (if (and v h d)
      (let [m (parse-device-str d)]
        (if (:error m)
          m
          (assoc (merge task m)
                 :Value   (value conf v)
                 :Wait    (wait conf w)
                 :Repeat  (rep conf r)
                 :NoReply (norepl conf n t))))
      {:error "missing <:Host>, <:Value>, <:Device> or invalid <:Device>"})))

(defmethod task :MODBUS
  [{conf :modbus} task]
  (let [{h :Host a :Address q :Quantity fc :FunctionCode
         w :Wait r :Repeat  v :Value    n :NoReply} task]
    (if (and h a fc)
      (assoc task
             :FunctionCode (fnc conf fc)
             :Value        (value conf v)
             :Address      (addr conf a)
             :Quantity     (quant conf q)
             :Repeat       (rep conf r)
             :Wait         (wait conf w)
             :NoReply      (norepl conf n))
      {:error "missing <:Host>, <:Address>, <:FuctionCode> "})))

(defmethod task :EXECUTE
  [{conf :execute} task]
  (let [{c :Cmd} task]
    (when c (assoc task :Cmd  (safe-cmd conf c)))))

(defn stub
  [{conf :stub} task]
  (let [{t :TaskName w :Wait r :Repeat v :Value} task]
    (assoc task
           :select (sel conf t)
           :Value  (value conf v)
           :Wait   (wait conf w)
           :Repeat (rep conf r))))
