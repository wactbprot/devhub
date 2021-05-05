(ns devhub.tcp
  ^{:author "Wact B. Prot <wactbprot@gmail.com>"
    :doc "Handles TCP Actions."}
  (:require [devhub.utils           :as u]
            [com.brunobonacci.mulog :as mu])
  (:import [java.io BufferedReader OutputStreamWriter InputStreamReader PrintWriter]
           [java.net Socket]))

(defn out-socket-raw [s] (.getOutputStream s)) ;; ship bytes

(defn out-socket [s] (PrintWriter. (OutputStreamWriter. (out-socket-raw s))))

(defn in-socket-raw [s]  (.getInputStream s))

(defn in-socket [s] (BufferedReader. (InputStreamReader. (in-socket-raw s))))

(defn gen-socket [{h :Host p :Port}] (Socket. h p))

(defn query
  "Sends the `cmds` given by the `:Value` to a raw tcp socket with the
  specified `host` and `port`.

  Example:
  ```clojure
  (def c (u/config))
  (def t {:Port 5025 :Host \"e75496\" :Value [\"room()\\n\"] :Wait 10 :Repeat 2})
  ;; prologix mks670
  (def t {:Port 1234 :Host \"192.168.98.204\" :Wait 10 :Repeat 10
  :Value [\"++addr 2\r++auto 1\r++eot_char 10\r:meas:func\r\"]})
  (query c t)
  ```"
  [{conf :tcp} {cmds :Value :as task}]
  (let [b? (bytes? (first cmds))]
    (if-not (u/connectable? task)
      {:error "can not connect"}
      (with-open [sock (gen-socket task)
                  out  (if b? (out-socket-raw sock) (out-socket sock))
                  in   (if b? (in-socket-raw sock) (in-socket sock))]
        (let [f (fn [cmd]
                  (when-not (empty? cmd)
                    (if b?
                      (.write out cmd 0 (count cmd))
                      (.print out cmd))
                    (.flush out)
                    (Thread/sleep (:read-delay conf)))
                  (if (:NoReply task) ""
                      (if b?
                        (into [] (for [_ (range (count cmd))] (.read in)))
                        (.readLine in))))]
          (u/run f conf task))))))


(defn handler
  "Handles TCP queries."
  [conf {host :Host port :Port req-id :req-id error :error :as task}]
  (mu/trace ::handler [:function "tcp/handler"]
            (if error
              task
              (let [_ (mu/log ::handler :req-id req-id :Host host :Port port)
                    data (query conf task)]
                (merge task (if (:error data)
                              (let [msg (:error data)]
                                (mu/log ::handler :error msg :req-id req-id)
                                data)
                              (let [msg "received data"]
                                (mu/log ::handler :message msg :req-id req-id)
                                (u/reshape data))))))))
