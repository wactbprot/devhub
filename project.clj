(defproject devhub "0.8.0"
  :description "Now a stub later a hub."
  :url "https://github.com/wactbprot/devhub"
  :license {:name "BSD 2-Clause"
            :url "https://github.com/wactbprot/devhub"}
  :dependencies [
                 [org.clojure/clojure                  "1.10.0"]
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
                 ;[clojang/codox-theme                  "0.2.0-SNAPSHOT"]
                 [com.intelligt.modbus/jlibmodbus      "1.2.9.7"]
                 ]
  :resource-paths ["resources"
                   "resources/js"
                   "resources/py"
                   "../jvxi11/external/jrpcgen.jar"    
                   "../jvxi11/external/oncrpc.jar"
                   "../jvxi11/external/one-jar-ant-task-0.97.jar"
                   "../jvxi11/external/portmap.jar"]
  :java-source-paths ["../jvxi11/"]
  :plugins [[lein-codox  "0.10.7"]
            [lein-cloverage  "1.1.2"]]
  :codox {;:themes [:clojang]
          :metadata {:doc/format :markdown}
          :source-uri "https://github.com/wactbprot/devhub/blob/master/{filepath}#L{line}"}
  :repl-options {:init-ns devhub.server}
  ;;:Main ^:skip-aot devhub.core
  :main devhub.server
  :aot [devhub.server]
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
