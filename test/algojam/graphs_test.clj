(ns algojam.graphs-test
  (:require [clojure.spec.test :as st]
            [clojure.spec.gen :as gen]
            [clojure.spec :as s]
            [clojure.test :as t]
            [algojam.graphs :as g]
            [algojam.graphs.generators :as ggen]
            [clojure.test.check.clojure-test :as tcct]
            [clojure.test.check.properties :as prop]
            [clojure.test.check.generators :as tcgen]))

(t/deftest -->>
  (t/testing "-->> with very general generators."
    (let [result (st/check `g/-->> {::clojure.spec.test.check/opts {:num-tests 50}})
          data   (-> result first :clojure.spec.test.check/ret)]
      (t/is (true? (:result data)) (str "Random seed is: " (:seed data)))))
  (t/testing "-->> with more narrowed generators"
    (let [result (st/check `g/-->> {::clojure.spec.test.check/opts {:num-tests 100}
                                    :gen                           {::g/node  (fn [] tcgen/nat)
                                                                    ::g/graph #(ggen/graph-gen tcgen/nat (gen/int))}})
          data   (-> result first :clojure.spec.test.check/ret)]
      (t/is (true? (:result data)) (str "Random seed is: " (:seed data))))))