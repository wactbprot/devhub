(defproject devhub "0.3.0"
  :description "Now a stub later a hub."
  :url "https://github.com/wactbprot/devhub"
  :license {:name "BSD 2-Clause"
            :url "https://github.com/wactbprot/devhub"}
  :dependencies [
                 [org.clojure/clojure       "1.10.0"]
                 [compojure                 "1.6.1"]
                 [http-kit                  "2.5.0"]
                 [cheshire                  "5.10.0"]
                 [ring/ring-defaults        "0.3.2"]
                 [ring/ring-core            "1.7.1"]
                 [ring/ring-devel           "1.7.1"]
                 [ring/ring-json            "0.5.0"]
                 [org.clojure/data.json     "1.0.0"]
                 [org.clojure/tools.logging "1.1.0"]
                 [clojang/codox-theme       "0.2.0-SNAPSHOT"]
                 ]
  :main ^:skip-aot devhub.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
