(ns graphs
  (:require [clojure.spec :as s]))

(defn -->> [graph node]
  (get graph node {}))

(s/fdef -->>
        :args (s/cat :graph ::graph :node ::node)
        :ret (s/coll-of ::node))

(defn -->? [graph start end]
  (contains? (-->> graph start) end))

(defn <-->? [graph node1 node2]
  (and (-->? graph node1 node2)
       (-->? graph node2 node1)))

(defn --> [graph start end weight]
  (let [start-->>    (-->> graph start)
        start-->end? (get start-->> end nil)
        start-->end  (if start-->end?
                       (conj start-->end? weight)
                       [weight])]
    (assoc graph start (assoc start-->> end start-->end))))

(defn <--> [graph node1 node2 weight]
  (-> graph
      (--> node1 node2 weight)
      (--> node2 node1 weight)))

(defn- simple-node-access [graph]
  (let [nodes (set (keys graph))]
    (every? (fn [node] (every? #(nodes %)
                               (-> graph (-->> node) keys set)))
            nodes)))

(s/def ::node number?)
(s/def ::edge-weights (s/coll-of number?))
(s/def ::graph (s/and (s/map-of ::node (s/map-of ::node ::edge-weights))
                      (s/spec simple-node-access)))

(defmacro graph-spec [node-spec edge-weights-spec])