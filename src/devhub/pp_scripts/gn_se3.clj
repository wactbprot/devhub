(ns devhub.pp-scripts.gn_se3
  (:require [devhub.pp-scripts.utils :as ppu]
            [devhub.utils            :as u]
            [jdk.nio.ByteBuffer      :as bb]
            [jdk.nio.ByteOrder       :as bo]))

(def conf (u/config "gn_se3.edn"))


(bb/get-float (bb/*wrap (byte-array [63 -86  0  0])))

