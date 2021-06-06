# µlog

* https://github.com/BrunoBonacci/mulog
* configuration:

TODO: link ennv var

```clojure
 :mulog {:type :multi
         :publishers[ 
                     ;; send events to the stdout
                      {:type :console
                       :pretty? true}
                     
					 ;; and/or
					 ;; send events to a file
                      {:type :simple-file
                       :filename "/tmp/mulog/events.log"}

	                 ;; and/or
					 ;; send events to ELS
                     {:type :elasticsearch
                      :url  "http://localhost:9200/"
                      :els-version  :v7.x
                      :publish-delay 1000
                      :data-stream  "vl-log-stream"
                      :name-mangling false}]}
```

* see [vl-log-stream](https://github.com/wactbprot/vl-log-stream)

