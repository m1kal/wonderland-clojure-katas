(ns tiny-maze.solver)

(defn- find-start [maze]
  (first
    (for [x (range 3) y (range 3) :when (= :S ((maze y) x))]
      [y x])))

(def ^:private directions [[-1 0] [0 1] [1 0] [0 -1]])

(defn- free-field-fn [maze]
  (fn [position]
    (if-let [value (get-in maze position)]
      (not= 1 value))))

(defn- neighbors [filter-fn position]
  (->> directions
       (map #(map + position %))
       (filter filter-fn)
       vec))

(defn- add-node-fn [filter-fn]
  (fn [path]
    (map #(conj (vec path) %)
      (remove (set path) (neighbors filter-fn (last path))))))

(defn- add-nodes [filter-fn paths]
  (->> paths
       (map (add-node-fn filter-fn))
       (apply concat)))

(defn- solution-fn [maze]
  (fn [path]
    (= :E (get-in maze (last path)))))

(defn solve-maze [maze]
  (loop [paths [[(find-start maze)]]]
    (if (empty? paths) nil
      (if-let [solution (first (filter (solution-fn maze) paths))]
        (reduce #(assoc-in %1 %2 :x) maze solution)
      (recur (add-nodes (free-field-fn maze) paths))))))

