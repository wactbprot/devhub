(ns devhub.tcp
  ^{:author "Thomas Bock <wactbprot@gmail.com>"
    :doc "Handles TCP Actions."}
  (:require [devhub.config :as c]
            [devhub.utils :as u]
            [com.brunobonacci.mulog :as µ]
            [clojure.string :as string]
            [wactbprot.vl-tcp :as tcp]))

(defn query
  "Sends the `cmds` given by the `:Value` to a raw tcp socket with the
  specified `host` and `port`. 

  Example:
  ```clojure
  (def c (c/config))
  (def t {:Port 5025 :Host \"e75496\" :Value [\"room()\\n\"] :Wait 10 :Repeat 2})
  ;; prologix mks670
  (def t {:Port 1234 :Host \"192.168.98.204\" :Wait 10 :Repeat 10
  :Value [\"++addr 2\r++auto 1\r++eot_char 10\r:meas:func\r\"]})
  (query c t)

  TODO: Introduce a number of bytes param e.G. `:NB`. At the moment
  `(tcp/rd-bytes in (count cmd))` assumes the number of bytes to
  read is the same as the number of bytes written.  
  ```"
  [{conf :tcp} {cmds :Value i :EOT n :NL h :Host p :Port :as task}]
  (let [b? (bytes? (first cmds))
        i? (int? i)
        l? (int? n)]
    (if-not (u/connectable? task)
      {:error "can not connect"}
      (with-open [sock (tcp/gen-socket h p)
                  out  (cond
                         b? (tcp/out-socket-raw sock)
                         :else (tcp/out-socket sock))
                  in   (cond
                         (or b? i?) (tcp/in-socket-raw sock)
                         :else (tcp/in-socket sock))]
        (let [f (fn [cmd]
                  (cond
                    (empty? cmd) nil
                    b? (tcp/wrt-bytes out cmd)
                    :else (tcp/wrt-str out cmd))
                  (Thread/sleep 200 #_(:read-delay conf))
                  (cond
                    (:NoReply task) nil
                    b? (tcp/rd-bytes in (count cmd)) 
                    i? (tcp/rd-eot in i)
                    l? (tcp/rd-lines in n)
                    :else (tcp/rd-line in)))]
          (u/run f conf task))))))

(defn handler
  "Handles TCP queries."
  [conf {error :error :as task}]
  (µ/trace ::handler [:function "tcp/handler"]
           (if error task
               (let [{error :error :as data} (query conf task)]
                 (merge task (if error data (u/reshape data)))))))
