(ns algojam.graphs
  (:require [clojure.spec :as s]
            [clojure.spec.gen :as gen]
            [algojam.graphs.generators :as gens]))

(s/def ::node any?)
(s/def ::edge-desc any?)

(s/def ::edges (s/map-of ::node ::edge-desc))
(s/def ::graph-structure (s/spec (s/map-of ::node ::edges)))

(s/def ::graph
  (s/spec (s/and ::graph-structure
                 (fn [graph] (let [nodes (set (keys graph))]
                               (every? (fn [node] (every? #(nodes %)
                                                          (-> graph (get node {}) keys set)))
                                       nodes))))
          :gen #(gens/graph-gen (s/gen ::node) (s/gen ::edge-desc))))

(defn -->> [graph node]
  (get graph node {}))

(s/fdef -->>
        :args (s/cat :graph ::graph :node ::node)
        :ret ::edges
        :fn #(let [args    (-> % :args)
                   graph   (:graph args)
                   node    (:node args)
                   node->> (graph node)]
               (or (not node->>)
                   (= (-> % :ret) node->>))))

(defn -->? [graph start end]
  (contains? (-->> graph start) end))

(defn <-->? [graph node1 node2]
  (and (-->? graph node1 node2)
       (-->? graph node2 node1)))

(defn -X-> [graph start end weight]
  (let [start-->>     (-->> graph start)
        weights       (get start-->> end [])
        new-weights   (filter #(not (= % weight)) weights)
        new-start-->> (if (empty? new-weights)
                        (dissoc start-->> end)
                        (assoc start-->> end new-weights))]
    (assoc graph start new-start-->>)))

(defn <-X-> [graph start end weight]
  (-> graph
      (-X-> start end weight)
      (-X-> end start weight)))

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

(defn ++ [graph node])

(defn -- [graph node])