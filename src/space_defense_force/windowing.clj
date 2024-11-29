(ns space-defense-force.windowing
  "I think this problem might best be framed as a windowing challenge."
  (:require [clojure.string :as str]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; a minimal example

(def dev:min-search-term
  "oo
o-")


(def dev:min-radar
  "-oo-
-o--
----")


(defn ->binary-vec [strs]
  (into []
        (map #(keep {\- 0 \o 1} %))
        (str/split-lines strs)))


(def min-search-term
  ;; [(1 1)
  ;;  (1 0)]
  (->binary-vec dev:min-search-term))

(def min-search-term2
  '[(1 1) 
    (1 0) 
    (0 0)])

(def min-radar
  ;; [(0 1 1 0)
  ;;  (0 1 0 0)
  ;;  (0 0 0 0)]
  (->binary-vec dev:min-radar))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; divvy search space into n x m squares

;; (ignoring edges for now)

(defn permutations
  "Returns rectangles n wide, m tall in colls"
  [n m colls]
  (mapcat (fn [xs]
            (apply map vector (map #(partition n 1 %)
                                   xs)))
          (partition m 1 colls)))

;; TODO way to preserve position

(comment
 (permutations 2 2 min-radar)

 (permutations 2 3 min-radar)
 ;; ([(0 1)
 ;;   (0 1)
 ;;   (0 0)]
 ;;  [(1 1)
 ;;   (1 0)
 ;;   (0 0)]
 ;;  [(1 0)
 ;;   (0 0)
 ;;   (0 0)])

 (some #{min-search-term} (permutations 2 2 min-radar))

 (some #{min-search-term2} (permutations 2 3 min-radar))
 )
