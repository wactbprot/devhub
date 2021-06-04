(ns devhub.pp-js-test
  (:require [devhub.config :as c]
            [clojure.test :refer :all]
            [devhub.pp-js :refer :all]))

(def pp [ "var _vec=_x.map(function(s){return s.split(' ')[1]}).map(parseFloat),",
         "_res = _.vlStat(_.checkNumArr(_vec).Arr),",
         "ToExchange={",
         "'@exchpath.Value':_res.mv,",
         "'@exchpath.SdValue':_res.sd,",
         "'@exchpath.N':_res.N,",
         "'@exchpath.Unit':'@unit',",
         "'@exchpath.Type':'@token'",
         "};",
         "var LogData={'vec':_vec, 't_start':_t_start, 't_stop':_t_stop}"])

(def data {:_x ["foo 1.1" "foo 2.2" "foo 3.3"]
           :_t_start [1 2 3]
           :_t_stop [1 2 3]})
 (comment
(deftest pp-js-i
  (testing " returns nil (i)"
    (is (nil? (pp-source nil nil))
        "nil .")
    (is (string? (pp-source pp data))
        "string")
    (is (map? (:ToExchange (exec (c/config) {:PostProcessing pp} data)))
        "exec"))))
