(ns build
  (:require [clojure.tools.build.api :as b]
            [devhub.meta :as m]))

(def lib 'com.github.wactbprot/devhub)
#_(def version (format "0.16.%s" (b/git-count-revs nil)))
(def class-dir "target/classes")
(def basis (b/create-basis {:project "deps.edn" :aliases [:dev]}))
(def uber-file (format "target/%s-%s-standalone.jar" (name lib) m/version))

(defn clean [_]
  (b/delete {:path "target"}))

(defn prep [_]
  (b/write-pom {:class-dir class-dir
                :lib lib
                :version m/version
                :basis basis
                :src-dirs ["src"]})
  (b/copy-dir {:src-dirs ["src" "resources"]
               :target-dir class-dir}))

(defn uber [_]
  (b/compile-clj {:basis basis
                  :src-dirs ["src"]
                  :class-dir class-dir})
  (b/uber {:class-dir class-dir
           :main 'devhub.server
           :uber-file uber-file
           :basis basis}))

(defn all [_]
  (clean nil)
  (prep nil)
  (uber nil))
