(ns devhub.meta
  ^{:author "Thomas Bock <wactbprot@gmail.com>"
    :doc "Syncs build and API version."}
  (:require [clojure.tools.build.api :as b]))


(def version (format "0.16.%s" (b/git-count-revs nil)))
