(ns algojam.graphs.generators
  (:require [clojure.spec.gen :as gen]
            [clojure.spec :as s]))

(defn- edges->graph [conns]
  (reduce (fn [graph [from to weight]]
            (let [from-->> (get graph from {})
                  ws       (get from-->> to [])]
              (if (nil? weight)
                (assoc
                  graph
                  from
                  from-->>)
                (assoc
                  graph
                  from
                  (assoc from-->>
                    to
                    (conj ws weight))))))
          {}
          conns))

(defn graph-gen [node-gen edge-gen]
  (gen/bind (gen/not-empty (gen/vector node-gen))
            (fn [nodes] (gen/fmap
                          edges->graph
                          (gen/vector
                            (gen/tuple
                              (gen/elements nodes)
                              (gen/elements nodes)
                              (gen/frequency [[8 edge-gen]
                                              [2 (gen/return nil)]])))))))