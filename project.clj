(defproject devhub "0.2.0"
  :description "Now a stub later a hub."
  :url "https://github.com/wactbprot/devhub"
  :license {:name "BSD 2-Clause"
            :url "https://github.com/wactbprot/devhub"}
  :dependencies [[org.clojure/clojure "1.10.0"]
                 [ring/ring-core "1.7.1"]
                 [ring/ring-jetty-adapter "1.7.1"]
                 [ring/ring-json "0.5.0"]
                 [ring/ring-devel "1.7.1"]]
  :main ^:skip-aot devhub.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
