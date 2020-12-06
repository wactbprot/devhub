(ns devhub.vxi11
  (:require [devhub.utils       :as u]
            [devhub.conf        :as c])
  (:import  [jvxi11 VXI11Factory VXI11UserFactory]))

(defn query
  [h d v w r]
  (let [ctrl (.create (VXI11Factory.) h d)
        usr  (.create (VXI11UserFactory.))]))

(defn handler
  "Handles TCP queries.
  
  Example:
  ```clojure
  (handler (:tcp (c/config))
           {:Wait 10 :Repeat 3 :Port 5025 :Host \"e75496\" :Value \"frs()\n\"}) 

  ```"
  [vxi-conf {w :Wait r :Repeat d :Device h :Host v :Value }]
  (if (and v h d )
    (if-let [data (query h d
                         (if (string? v) [v] v)
                         (if w (u/number w) (:min-wait vxi-conf))
                         (if r (u/number r) (:repeat vxi-conf)))]
      {:data (u/meas-vec data)}
      {:error true :reason "no data"})
    {:error true :reason "missing <value>, <host> or <device>"}))
