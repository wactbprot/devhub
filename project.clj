(defproject devhub "0.3.0"
  :description "Now a stub later a hub."
  :url "https://github.com/wactbprot/devhub"
  :license {:name "BSD 2-Clause"
            :url "https://github.com/wactbprot/devhub"}
  :dependencies [[org.clojure/clojure "1.10.0"]
                 [compojure "1.6.1"]
                 [aleph "0.4.7-alpha5"]
                 [manifold "0.1.9-alpha4"]
                 [ring/ring-json "0.5.0"]
                 [clojang/codox-theme "0.2.0-SNAPSHOT"]
                 ]
  :main ^:skip-aot devhub.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
