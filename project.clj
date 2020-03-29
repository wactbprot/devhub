(defproject devhub "0.1.0-SNAPSHOT"
  :description "Now a stub later a hub."
  :url "http://example.com/FIXME"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.10.0"]
                 [compojure "1.6.1"]
                 [ring/ring-core "1.6.3"]
                 [ring/ring-jetty-adapter "1.6.3"]
                 [ring/ring-json "0.5.0"]
                 [ring/ring-devel "1.6.3"]]
  :main ^:skip-aot devhub.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
