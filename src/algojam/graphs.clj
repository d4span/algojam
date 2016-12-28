(ns algojam.graphs
  (:require [clojure.spec :as s]
            [clojure.spec.gen :as gen]))

(s/def ::node any?)

(defn- comparable? [x] (instance? Comparable x))
(s/def ::Comparable (s/spec comparable?
                            :gen (fn [] (gen/one-of [(gen/string)
                                                     (gen/int)
                                                     (gen/double)
                                                     (gen/list (gen/any-printable))
                                                     (gen/vector (gen/any-printable))]))))

(s/def ::edge-weight ::Comparable)

(s/def ::edge-weights (s/coll-of ::edge-weight))
(s/def ::graph-structure (s/spec (s/map-of ::node (s/map-of ::node ::edge-weights))
                                 :gen (fn [] (gen/fmap
                                               (fn [conns] (reduce (fn [graph [from to weights]]
                                                                     (let [from-->> (get graph from {})
                                                                           ws       (get from-->> to [])]
                                                                       (assoc
                                                                         graph
                                                                         from
                                                                         (assoc from-->>
                                                                                to
                                                                                (concat ws weights)))))
                                                                   {}
                                                                   conns))
                                               (gen/vector (gen/tuple
                                                             (s/gen ::node)
                                                             (s/gen ::node)
                                                             (gen/vector (s/gen ::edge-weight))))))))

(s/def ::graph (s/and ::graph-structure
                      (fn [graph] (let [nodes (set (keys graph))]
                                    (every? (fn [node] (every? #(nodes %)
                                                               (-> graph (get node {}) keys set)))
                                            nodes)))))

(defn -->> [graph node]
  (get graph node {}))

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