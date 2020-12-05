(ns devhub.post-test
  (:require [clojure.test :refer :all]
            [devhub.post :refer :all]
            ))
(def pp [
         "var _vec=_x.map(function(s){return s.split(' ')[1]}).map(parseFloat),",
         "_res = _.vlStat(_.checkNumArr(_vec).Arr),",
         "ToExchange={",
         "'@exchpath.Value':_res.mv,",
         "'@exchpath.SdValue':_res.sd,",
         "'@exchpath.N':_res.N,",
         "'@exchpath.Unit':'@unit',",
         "'@exchpath.Type':'@token'",
         "};",
         "var LogData={'vec':_vec, 't_start':_t_start, 't_stop':_t_stop}"
         ])

(deftest js-pp-i
  (testing " returns (i)"
    (is (nil? (js-pp nil nil))
        "nil .")))
