(defproject devhub "0.12.0"
  :description "Once a stub now a hub."
  :url "https://github.com/wactbprot/devhub"
  :license {:name "BSD 2-Clause"
            :url "https://github.com/wactbprot/devhub"}
  :dependencies [[org.clojure/clojure                  "1.10.1"]
                 [compojure                            "1.6.1"]
                 [http-kit                             "2.5.0"]
                 [cheshire                             "5.10.0"]
                 [ring/ring-defaults                   "0.3.2"]
                 [ring/ring-core                       "1.7.1"]
                 [ring/ring-devel                      "1.7.1"]
                 [ring/ring-json                       "0.5.0"]
                 [org.clojure/data.json                "1.0.0"]
                 [com.brunobonacci/mulog               "0.6.0"]
                 [com.brunobonacci/mulog-elasticsearch "0.6.0"]
                 [clojure-interop/java.nio             "1.0.5"]
                 [com.intelligt.modbus/jlibmodbus      "1.2.9.7"]]
  :resource-paths ["resources"
                   "resources/js"
                   "resources/py"
                   "../jvxi11/external/jrpcgen.jar"
                   "../jvxi11/external/oncrpc.jar"
                   "../jvxi11/external/one-jar-ant-task-0.97.jar"
                   "../jvxi11/external/portmap.jar"]
  :java-source-paths ["../jvxi11/"]
  :plugins [[lein-codox  "0.10.7"]
            [lein-cloverage  "1.1.2"]
            [lein-kibit "0.1.8"]]
  :codox {:metadata {:doc/format :markdown}
          :source-uri "https://github.com/wactbprot/devhub/blob/master/{filepath}#L{line}"}
  :repl-options {:init-ns devhub.server}
  :main devhub.server
  :aot [devhub.server]
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
