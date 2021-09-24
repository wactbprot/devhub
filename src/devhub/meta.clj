(ns devhub.meta
  (:require [clojure.tools.build.api :as b]))


(def version (format "0.16.%s" (b/git-count-revs nil)))
