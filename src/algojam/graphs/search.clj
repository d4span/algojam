(ns algojam.graphs.search
  (:require [algojam.graphs :as g]))

(defn bfs
  ([graph queue visited result]
   (let [node          (first queue)
         neighbors     (vec (keys (g/-->> graph node)))
         new-neighbors (filter #(not (visited %)) neighbors)
         new-queue     (lazy-cat (drop 1 queue) new-neighbors)
         edges         (map (fn [neighbor] [node neighbor]) new-neighbors)
         new-result    (concat result edges)]
     (if (empty? new-queue)
       new-result
       (recur graph new-queue (conj visited node) new-result))))
  ([graph node]
   (bfs graph [node] #{} [])))