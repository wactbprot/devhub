(ns devhub.pp-scripts.vacom
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

(defn str-payload [v] (subvec v (:start-payload-str conf) (:stop-payload-str conf)))

(defn payload->str [v] (string/join (map (comp str char) v)))

(defn extract [s]
  (let [r #"[+-]*[0-9]*\.[0-9]*[E][-+][0-9]*"]
    (re-matches r s)))

(defn count-ok? [task] (= (:byte-count conf) (count (:_x task))))

(defn check-respons [task]
  (merge task {:ToExchange {:Response {:ok (count-ok? task)}}}))

(defn read-pressure [task]
  (let [input (:PostScriptInput task)
        v      (mapv #(extract (payload->string (str-payload %))) (:_x task))
        o     (ppu/operable v)]
    (merge task {:Result [(ppu/vl-result (:Type input) (ppu/calc-seq v o) "mbar")]})))

(defn read-pressure-vec [task]
  (let [input (:PostScriptInput task)
        v      (mapv #(extract-value (payload->str (str-payload %))) (:_x task))
        o     (ppu/operable v)]
    (merge task {:Result [{:Type (:Type input) :Value (ppu/calc-seq v o) :Unit "mbar"}]})))

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
