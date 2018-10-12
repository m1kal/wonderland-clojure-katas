(ns alphabet-cipher.coder
 (:require [clojure.string :refer [join]]))

(defn- char-to-pos [x]
  (- (int x) (int \a)))

(defn- lookup [x y dir]
  (->> [x y]
       (map char-to-pos)
       (map * [dir 1])
       (apply + 26)
       (#(mod % 26))
       (+ (int \a))
       char))

(defn- extend-keyword [keyword length]
  (join (repeat length keyword)))

(defn- code [keyword message dir]
  (->> message
       (map
         #(lookup %1 %2 dir)
         (extend-keyword keyword (count message)))
         join))

(defn encode [keyword message]
  (code keyword message 1))

(defn decode [keyword message]
  (code keyword message -1))

(defn- repeats? [pattern input]
  (empty?
    (filter
      #(not (every? identity (map = pattern %)))
      (partition-all (count pattern) input))))

(defn- extract [x]
  (loop [out (str (first x)) r (rest x)]
    (if (empty? r)
         out
         (if (repeats? out r)
           out
           (recur (str out (first r)) (rest r))))))

(defn decipher [cipher message]
  (extract (decode message cipher)))

