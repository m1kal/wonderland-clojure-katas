(ns doublets.solver
  (:require [clojure.java.io :as io]))

(def words (-> "words.edn"
               (io/resource)
               (slurp)
               (read-string)))

(defn- hamming-distance [x y]
  (and
    (= (count x) (count y))
    (->> [x y]
         (apply map not=)
         (filter identity)
         count)))

(defn- next-steps [dict x]
  (map
    #(conj x %)
    (filter
      #(= 1 (hamming-distance (last x) %))
      dict)))

(defn doublets [word1 word2]
  (loop [path [[word1]] n (count words)]
    (let [matching-path (filter #(some #{word2} %) path)]
      (or
        (first matching-path)
        (if (< n 0) [])
        (recur (apply concat (map #(next-steps words %) path)) (dec n))))))

