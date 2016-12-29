(ns algojam.graphs-test
  (:require [clojure.spec.test :as st]
            [clojure.test :as t]
            [algojam.graphs :as g]))

(t/deftest -->>
  (let [result (st/check `g/-->> {::clojure.spec.test.check/opts {:num-tests 24}})
        data   (-> result first :clojure.spec.test.check/ret)]
    (t/is (true? (:result data)) (str "Random seed is: " (:seed data)))))