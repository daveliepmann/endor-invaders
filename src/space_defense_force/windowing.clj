(ns space-defense-force.windowing
  "I think this problem might best be framed as a windowing challenge."
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

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
        (map #(into [] (keep {\- 0 \o 1} %)))
        (str/split-lines strs)))


(def min-search-term
  ;; [[1 1]
  ;;  [1 0]]
  (->binary-vec dev:min-search-term))

(def min-search-term2
  '[[1 1] 
    [1 0] 
    [0 0]])

(def min-radar
  ;; [[0 1 1 0]
  ;;  [0 1 0 0]
  ;;  [0 0 0 0]]
  (->binary-vec dev:min-radar))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; divvy search space into n x m squares

;; (ignoring edges for now)

(defn permutations
  "Returns rectangles n wide, m tall in colls"
  [n m colls]
  (mapcat (fn [xs]
            (apply mapv vector (mapv #(partitionv n 1 %)
                                     xs)))
          (partition m 1 colls)))

(comment
 (permutations 2 2 min-radar)

 (permutations 2 3 min-radar)
 ;; ([[0 1]
 ;;   [0 1]
 ;;   [0 0]]
 ;;  [[1 1]
 ;;   [1 0]
 ;;   [0 0]]
 ;;  [[1 0]
 ;;   [0 0]
 ;;   [0 0]])

 (some #{min-search-term} (permutations 2 2 min-radar))

 (some #{min-search-term2} (permutations 2 3 min-radar))
 )


(defn permutations-by-xy
  "Returns rectangles n wide, m tall in colls, keyed by original xy coordinates"
  [[n m :as _dimensions] colls]
  (into {}
        (mapcat (fn [[y-idx xs]]
                  (map-indexed (fn [x-idx itm] {[x-idx y-idx] itm})
                               (apply mapv vector (mapv #(partitionv n 1 %)
                                                        xs)))))
        (map-indexed #(vector % %2)
                     (partition m 1 colls))))

(comment
  (permutations-by-xy [2 2] min-radar)
  ;; {[0 0] [[0 1]
  ;;         [0 1]],
  ;;  [0 1] [[0 1]
  ;;         [0 0]],
  ;;  [1 0] [[1 1]
  ;;         [1 0]],
  ;;  [1 1] [[1 0]
  ;;         [0 0]],
  ;;  [2 0] [[1 0]
  ;;         [0 0]],
  ;;  [2 1] [[0 0]
  ;;         [0 0]]}
 
 (permutations-by-xy [2 3] min-radar)
 ;; {[0 0] [[0 1]
 ;;         [0 1]
 ;;         [0 0]],
 ;;  [1 0] [[1 1]
 ;;         [1 0]
 ;;         [0 0]],
 ;;  [2 0] [[1 0]
 ;;         [0 0]
 ;;         [0 0]]}
 
 )

(defn fuzzy-match
  "Returns rectangle `rect` if `shape` matches its signal within `tolerance`"
  [shape rect tolerance]
  (let [xs (range 0 (count rect))
        ys (range 0 (count (first rect)))]
    (when (>= tolerance
              (->> (for [x xs, y ys] [x y])  ; all combos
                   (map (fn [[x y]] (= (get-in rect [x y])
                                      (get-in shape [x y]))))
                   (filter false?)
                   count))
      rect)))

(defn fuzzy-match2
  "Returns XY coordinates of `rect` if `shape` matches its signal within `tolerance`"
  [shape [xy rect] tolerance]
  (let [xs (range 0 (count rect))
        ys (range 0 (count (first rect)))]
    (when (>= tolerance
              (->> (for [x xs, y ys] [x y])  ; all combos
                   (map (fn [[x y]] (= (get-in rect [x y])
                                      (get-in shape [x y]))))
                   (filter false?)
                   count))
      xy)))

(comment ;;;; fuzzy match

  (fuzzy-match min-search-term2 [[0 1] [0 1] [0 0]] 2) ; nil
  (fuzzy-match min-search-term2 [[0 1] [0 1] [0 0]] 3) ; [[0 1] [0 1] [0 0]]
  
  
  (let [shape min-search-term2
        search-space min-radar
        tolerance 1]
    (some (fn [rect] (fuzzy-match shape rect 3))
          (permutations 2 3 search-space)))

  (let [shape min-search-term2
        search-space min-radar
        tolerance 1]
    (some (fn [[xy rect]] (fuzzy-match2 shape [xy rect] 1))
          (permutations-by-xy [2 3] search-space)))
  
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;;;; apply to sample dats

(def radar-sample
  (->binary-vec (slurp (io/resource "samples/radar1"))))

(def invader1 (->binary-vec (slurp (io/resource "samples/invader1"))))
(def invader2 (->binary-vec (slurp (io/resource "samples/invader2"))))

(defn dims
  "Returns width and height of rectangular vector of vectors `shape`"
  [shape]
  ;; TODO assert rectangularity and vectorness
  [(count (first shape))
   (count shape)])

(comment
   (let [shape invader1
        search-space radar-sample
        tolerance 12 #_ 8
        xy (dims shape)]
    (keep #_some (fn [[xy rect]] (fuzzy-match2 shape [xy rect] tolerance))
         (permutations-by-xy xy search-space)))

  (let [shape invader2 #_ invader1
        search-space radar-sample
        tolerance 8
        xy (dims shape)]
    (keep (fn [[xy rect]] (fuzzy-match2 shape [xy rect] tolerance))
         (permutations-by-xy xy search-space)))
  
  )
