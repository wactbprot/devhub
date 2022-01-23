(ns devhub.pp-scripts.vacom
  ^{:author "Thomas Bock <wactbprot@gmail.com>"
    :doc "Pre/Post processing for vacom protocol."}
  (:require [devhub.pp-utils :as ppu]
            [clojure.string :as string]
            [devhub.utils :as u]))

(def conf {:read-mbar-str [0xA5 0x50 0x00 0x00
                           0x20 0x10 ;; request actual value
                           0x01 0x00 0x00 0x00 0x00 0x00 0x00 0x00 0x00 0x00
                           0x00 0x00 0x00 0x00 0x00 0x00
                           0xFF 0x8C]
           :start-payload-str 6
           :stop-payload-str 18
           :byte-count 24})


(defn meas-pressure [task]
  (assoc task :Value [(byte-array (:read-mbar-str conf))]))

(defn str-payload [v]
  (subvec v (:start-payload-str conf) (:stop-payload-str conf)))

(defn payload->str [v]
  (string/join (map (comp str char) v)))

(defn extract [s]
  (let [r #"[+-]*[0-9]*\.[0-9]*[E][-+][0-9]*"]
    (re-matches r s)))

(defn count-ok? [{x :_x}] (= (:byte-count conf) (count x)))

(defn check-response [task]
  (merge task {:ToExchange {:Response {:ok (count-ok? task)}}}))

(defn val-vec [{x :_x}]
  (let [v (mapv #(extract (payload->str (str-payload %))) x)]
    (ppu/calc-seq v (ppu/operable v))))

(defn read-pressure [task]
  (merge task {:Result [(ppu/vl-result (get-in task [:PostScriptInput :Type])
                                       (val-vec task)
                                       "mbar")]}))

(defn read-pressure-vec [task]
  (merge task {:Result [{:Type (get-in task [:PostScriptInput :Type])
                         :Value (val-vec task)
                         :Unit "mbar"}]}))

(comment
  (def b
    [0xA5 0x50 0x00 0x00
     0x20 0x10 ;; request actual value
     0x01 0x00 0x00 0x00 0x00 0x00 0x00 0x00 0x00 0x00
     0x00 0x00 0x00 0x00 0x00 0x00
     0xFF 0x8C])
  ;; answer
  (def v [165 112 0 0 32 16
          51 46 54 50 48 53 53 54 69 45 48 50 0 0 0 0 87 248])
  
  (def b
    [0xA5 0x50 0x00 0x00
     0x01 0x00 ;; type string
     0x00 0x00 0x00 0x00 0x00 0x00 0x00 0x00 0x00 0x00
     0x00 0x00 0x00 0x00 0x00 0x00
     0x69 0xEF])
    
  (def a [0xA5 0x70 0x00 0x00 0x01 0x00
          0x41 0x74 0x6D 0x69 0x67 0x72 0x61 0x66 0x31 0x30 0x30 ; atmigraf100
          0x00 0x00 0x00 0x00 0x00 0xC8 0xCE]))
