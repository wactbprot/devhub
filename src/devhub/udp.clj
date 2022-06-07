(ns devhub.udp
  ^{:author "Thomas Bock <wactbprot@gmail.com>"
    :doc "Handles UDP Actions."}
  (:require [devhub.utils :as u]
            [devhub.safe :as safe]
            [com.brunobonacci.mulog :as µ]
            [udp-wrapper.core :as udp]))

(defn query
  "Sends the `cmds` to a udp device."
  [{conf :udp} {host :Host port :Port value :Value :as task}]
  (let [socket (udp/create-udp-server (:send-buffer-size conf))
        host   (udp/make-address host)
        f      #(udp/send-message socket (udp/packet (udp/get-bytes-utf8 %) host port))
        data   (u/run f conf task)
        _      (udp/close-udp-server socket)]
    data))

(defn handler
  "Handles UDP queries. "
  [conf {error :error :as task}]
  (µ/trace ::handler [:function "udp/handler"]
           (if error task
               (let [{error :error :as data} (query conf task)]
                 (merge task (if error data (u/reshape data)))))))
  
