(ns space-defense-force.radar
  "Invader-detection API

  Fns to reveal possible locations of known hostiles in ASCII radar
  signals. Representative signals are in `resources/samples`.

  API consumers: see 'Recommended public API' and `invaders->matches`. Fns
  supporting detection of partial shapes at the edges work, but are not yet
  integrated in the main detection fn. See `invader's-right-edge-detector` &
  friends.

  A Match is a map describing an area of interest. It has keys:
   `:known-shape` - input parameter describing possibly-detected invader rectangle
   `:tolerance` - input parameter which allowed the fuzzy match
   `:match` - the rectangle in the signal which may match a known invader
   `:coords` - x/y coordinates of the match (optional at certain stages)
   `:dims` - width/height dimensions of the match (optional at certain stages)
   `noise-count` - number of points in the matching rectangle which did not match the known shape
   `score` - relative value of the match (higher is better)

  Implementation notes
   - all coordinates are from top left
   - treated as a windowing task
     - the shape of the problem suggests there may be an elegant recursive solution"
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Utilities

(defn dims
  "Returns width and height of rectangular vector of vectors `shape`"
  [shape]
  (when-not (and (vector? shape) (every? vector? shape))
    (throw (ex-info "dims not defined if shape is not vector of vectors"
                    {:kind :fail/illegal-argument, :shape shape})))
  (when-not (apply = (map count shape))
    (throw (ex-info "dims not defined for non-rectangular shapes"
                    {:kind :fail/illegal-argument, :counts (map count shape)})))
  [(count (first shape))
   (count shape)])


(defn ->binary-vec
  "Conversion fn: returns vector of vectors of 0/1 from newlined String
  (Working with strs/chars did not spark joy - DL_2024-12-02)"
  [strs]
  (into []
        (map #(into [] (keep {\- 0 \o 1
                              ;; XXX sample radar includes an uppercase \O - we
                              ;; assume this is a typo and treat it as
                              ;; lowercase:
                              \O 1} %)))
        (str/split-lines strs)))


(defn ->signal-str
  "Conversion fn: returns newlined String from vector of vectors of 0/1
  Useful in case consumers prefer answers in the original radar format."
  [binary-vec]
  (str/join "\n" (into [] ; TODO compare perf w/`->>` against real data
                       (comp (map #(into [] (keep {0 \- 1 \o} %)))
                             (map #(apply str %)))
                       binary-vec)))


(comment ;;; Sample data for REPL debugging
  (def dbg:radar-sample
    (->binary-vec (slurp (io/resource "samples/radar1"))))

  (def dbg:invader1 (->binary-vec (slurp (io/resource "samples/invader1"))))
  (def dbg:invader2 (->binary-vec (slurp (io/resource "samples/invader2"))))

  (= dbg:invader1 (->binary-vec (->signal-str dbg:invader1)))
  (= dbg:invader2 (->binary-vec (->signal-str dbg:invader2)))

  ;; aside: note the aberrant uppercase 'O', so can't roundtrip radar1
  (distinct (mapcat #(slurp (io/resource %))
                    ["samples/invader1"
                     "samples/invader2"
                     "samples/radar1"]))
  ;; => (\- \o \newline \O)
  
  ;; conversion example
  ;; [[0 0 0 1 1 0 0 0]
  ;;  [0 0 1 1 1 1 0 0]
  ;;  [0 1 1 1 1 1 1 0]
  ;;  [1 1 0 1 1 0 1 1]
  ;;  [1 1 1 1 1 1 1 1]
  ;;  [0 0 1 0 0 1 0 0]
  ;;  [0 1 0 1 1 0 1 0]
  ;;  [1 0 1 0 0 1 0 1]]
  (spit "resources/out2" (->signal-str dbg:invader2))
  ;; ---oo---
  ;; --oooo--
  ;; -oooooo-
  ;; oo-oo-oo
  ;; oooooooo
  ;; --o--o--
  ;; -o-oo-o-
  ;; o-o--o-o
  
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Implementation

(defn permutations-by-xy
  "Returns `n`-wide `m`-tall sub-rectangles in `rectangle`,
  keyed by xy coordinates in the original"
  [[n m :as _dimensions] rectangle]
  (into {}
        (mapcat (fn [[y-idx xs]]
                  (map-indexed (fn [x-idx itm] {[x-idx y-idx] itm})
                               (apply mapv vector (mapv #(partitionv n 1 %)
                                                        xs)))))
        (map-indexed #(vector % %2)
                     (partition m 1 rectangle))))


(defn fuzzy-match
  "Returns map describing matches of `shape`'s signal within `tolerance` in `rect`"
  [shape rect tolerance]
  (let [xs (range 0 (count rect))
        ys (range 0 (count (first rect)))
        noise-count (->> (for [x xs, y ys] [x y])  ; all combos
                         (map (fn [[x y]] (= (get-in rect [x y])
                                            (get-in shape [x y]))))
                         (filter false?)
                         count)]
    (when (>= tolerance noise-count)
      {:known-shape shape, :match rect
       :tolerance tolerance, :noise-count noise-count})))


(comment ;;; How to use

  (fuzzy-match dbg:invader1 dbg:invader1 0) ; tautological case

  ;; very small rects can be visually verified
  (fuzzy-match [[1 1]
                [1 0]]
               [[1 1]
                [1 1]]
               1 #_0)

  (fuzzy-match [[1 1] 
                [1 0] 
                [0 0]]
               [[1 1]
                [1 0]
                [0 1]]
               1)

  (fuzzy-match dbg:invader1
               ;; dbg:invader1 with a couple manually-inserted changes:
               [[1 0 1 0 0 0 0 0 1 0 0]
                [0 0 0 1 0 0 0 1 0 0 0]
                [0 0 1 1 1 1 1 1 1 0 0]
                [0 1 1 0 1 1 1 0 1 1 0]
                [1 1 1 1 1 1 1 1 1 1 1]
                [1 0 1 1 1 1 1 1 1 0 1]
                [1 0 1 0 1 0 0 0 1 0 1]
                [0 0 0 1 1 0 1 1 0 0 0]]
               2 #_1)    
  
  )


(defn assoc-score
  "Given a Match map, adds a :score key with the relative value of the match"
  [{:keys [dims noise-count] :as match}]
  (let [[n m] dims]
    (assoc match :score (double (if (pos? noise-count)
                                  (/ (* n m) noise-count)
                                  (* n m))))))


(defn invader-matches
  "Returns matches of invader `shape` in `search-space` within `tolerance`"
  [shape search-space tolerance]
  (let [dimensions (dims shape)]
    (keep (fn [[xy rect]] (when-let [m (fuzzy-match shape rect tolerance)]
                           (assoc m
                                  :coords xy
                                  :dims dimensions)))
          (permutations-by-xy dimensions search-space))))

(comment
  (map assoc-score (invader-matches dbg:invader1 dbg:invader1 8))
  (map assoc-score (invader-matches dbg:invader1 dbg:radar-sample 12))

  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; edges

(comment ;;;; edge case exploration
  ;;; minimum example
  ;; search space
  [[1 0 1]
   [1 1 1]
   [0 0 1]]

  ;; want to detect
  [[1] [1] [0]]

  (invader-matches [[1] [1] [0]]
                   [[1 0 1]
                    [1 1 1]
                    [0 0 1]]
                   0)

  ;; kind of annoying to handle its presence elsewhere, e.g.
  (invader-matches [[1] [1] [0]]
                   [[1 1 1]
                    [1 1 1]
                    [0 0 1]]
                   0)

  ;; let's try to grab just the left edge of the known shape,
  ;; and the corresponding right edge of the search space
  (let [radar [[1 1 1]
               [1 1 1]
               [0 0 1]]
        invader [[1 0 1]
                 [1 0 1]
                 [1 0 0]]
        edge-width 1
        invader-right-edge (get (permutations-by-xy [edge-width (count invader)] invader)
                                [(dec (count (first invader)))
                                 0])
        radar-left-edge (get (permutations-by-xy [edge-width (count invader)] radar)
                              [0 0])]
     (invaders->matches [invader-right-edge] radar-left-edge 0))

  ;; let's extend this to implementation
  )


(defn invader's-right-edge-detector
  "Detects the `width` right columns of `shape` on the left edge of `search-space`, allowing for `tolerance` mismatches"
  [shape search-space tolerance width]
  (let [invader-right-edge (get (permutations-by-xy [width (count shape)] shape)
                                [(- (count (first shape)) width)
                                 0])
        radar-left-edge (get (permutations-by-xy [width (count search-space)] search-space)
                              [0 0])]
    (invader-matches invader-right-edge radar-left-edge tolerance)))


(comment
  ;; minimum example: exact match of single col
  (invader's-right-edge-detector [[0 1 1]
                                  [0 1 1]
                                  [0 0 1]]
                                 [[1 0 1]
                                  [1 0 1]
                                  [1 0 0]]
                                 0 1)
  
  ;; mismatch height
  (invader's-right-edge-detector [[0 1 1]
                                  [0 1 1]]
                                 [[0 0 0]
                                  [1 0 0]
                                  [1 0 0]]
                                 0 1)

  ;; mismatch height w/more width
  (invader's-right-edge-detector [[0 1 1]
                                  [0 1 1]]
                                 [[0 0 0]
                                  [1 1 0]
                                  [0 1 0]]
                                 1 2)
  
  ;; multiple matches
  (invader's-right-edge-detector [[0 1 1]
                                  [0 1 1]]
                                 [[1 1 1]
                                  [1 1 1]
                                  [1 1 1]
                                  [0 1 1]]
                                 ;; note: only 2 matches with 0 tolerance
                                 1 2)

  )


(defn invader's-left-edge-detector
  "Detects the `width` left columns of `shape` on the right edge of `search-space`, allowing for `tolerance` mismatches"
  [shape search-space tolerance width]
  (let [invader-right-edge (get (permutations-by-xy [width (count shape)] shape)
                                [0 0])
        radar-left-edge (get (permutations-by-xy [width (count search-space)] search-space)
                              [(- (count (first search-space)) width)
                               0])]
    (invader-matches invader-right-edge radar-left-edge tolerance)))

(comment
  ;; minimum example: exact match of single col
  (invader's-left-edge-detector [[0 1 1]
                                 [0 1 1]
                                 [0 1 1]]
                                [[1 1 0]
                                 [1 1 0]
                                 [1 1 0]]
                                0 1)

  ;; mismatch height
  (invader's-left-edge-detector [[1 1 1]
                                 [1 1 1]]
                                [[0 0 0]
                                 [0 0 1]
                                 [0 0 1]]
                                0 1)

  ;; multiple matches
  (invader's-left-edge-detector [[1 1 1]
                                 [1 1 1]]
                                [[0 1 1]
                                 [0 1 1]
                                 [0 1 1]
                                 [0 0 1]]
                                ;; note: only 2 matches with 0 tolerance
                                1 2)

  )


(defn invader's-top-edge-detector
  "Detects the `height` top columns of `shape` on the bottom edge of `search-space`, allowing for `tolerance` mismatches"
  [shape search-space tolerance height]
  (let [invader-top-edge (get (permutations-by-xy [(count (first shape)) height] shape)
                                [0 0])
        radar-bottom-edge (get (permutations-by-xy [(count (first search-space)) height] search-space)
                                [0
                                 (- (count search-space) height)])]
    (invader-matches invader-top-edge radar-bottom-edge tolerance)))

(comment
  ;; minimum example: exact match of single row
  (invader's-top-edge-detector [[1 1 1]
                                [0 0 0]
                                [0 0 0]]
                               [[0 0 0]
                                [0 0 0]
                                [1 1 1]]
                                0 1)
  
  ;; mismatch width
  (invader's-top-edge-detector [[1 1]
                                [0 0]]
                               [[0 0 0]
                                [0 0 1]
                                [0 1 1]]
                               0 1)

  ;; multiple matches
  (invader's-top-edge-detector [[1 1]
                                [0 0]]
                                [[0 0 0 0]
                                 [0 0 0 0]
                                 [0 0 0 0]
                                 [0 1 1 1]]
                                ;; note: only 2 matches with 0 tolerance
                                0 1)

  ;; deeper height
  (invader's-top-edge-detector [[1 1]
                                [1 1]]
                               [[0 0 0 0]
                                [0 0 0 0]
                                [1 1 1 1]
                                [0 1 0 1]]
                               ;; note: 0 matches with 0 tolerance
                               1 #_0 2)
  
  )


(defn invader's-bottom-edge-detector
  "Detects the `height` bottom columns of `shape` on the top edge of `search-space`, allowing for `tolerance` mismatches"
  [shape search-space tolerance height]
  (let [invader-bottom-edge (get (permutations-by-xy [(count (first shape)) height] shape)
                                 [0 (- (count shape) height)])
        radar-top-edge (get (permutations-by-xy [(count (first search-space)) height] search-space)
                                [0 0])]
    (invader-matches invader-bottom-edge radar-top-edge tolerance)))

(comment
  ;; minimum example: exact match of single row
  (invader's-bottom-edge-detector [[0 0 0]
                                   [1 1 1]]
                                  [[1 1 1]
                                   [0 0 0]
                                   [0 0 0]]
                                  0 1)
  
  ;; mismatch width
  (invader's-bottom-edge-detector [[0 0]
                                   [1 1]]
                                  [[0 1 1]
                                   [0 0 0]
                                   [0 0 0]]
                                  0 1)

  ;; multiple matches
  (invader's-bottom-edge-detector [[0 0]
                                   [1 1]]
                                  [[0 1 1 1]
                                   [0 0 0 0]
                                   [0 0 0 0]
                                   [0 0 0 0]]
                                  ;; note: 2 matches with 0 tolerance; 3 w/1
                                  0 1)

  ;; deeper height
  (invader's-bottom-edge-detector [[0 0]
                                   [1 1]
                                   [1 1]]
                                  [[1 1 1 1]
                                   [1 1 0 1]
                                   [0 0 0 0]
                                   [0 0 0 0]]
                                  ;; note: 1 match with 0 tolerance
                                  1 2)
  
  )

;; TODO corners



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Recommended public API

(defn invaders->matches
  "Returns matches for any invader `shapes` in the `search-space`
  Allows for `tolerance` number of mismatches points to account for noise."
  [shapes search-space tolerance]
  (->> shapes
       (mapcat #(invader-matches % search-space tolerance))
       (map assoc-score)
       (sort-by :score)))

(comment ;; basic usage
  (invaders->matches [dbg:invader1 dbg:invader2]
                     dbg:radar-sample
                     15)


  )

(comment ;;; integrating edge cases into main detection fn: experiments
  (let [inv-2x4 [[0 0]
                 [0 0]
                 [1 0]
                 [0 1]]
        search-space [[0 1 0 0]
                      [0 1 0 0]
                      [0 1 0 0]
                      [1 0 1 0]
                      [0 0 0 1]]
        tolerance 0]
    (concat (invaders->matches [inv-2x4] search-space tolerance)
            (invader's-right-edge-detector inv-2x4 search-space tolerance 1)
            (invader's-left-edge-detector inv-2x4 search-space tolerance 1)
            (invader's-top-edge-detector inv-2x4 search-space tolerance 1)
            (invader's-bottom-edge-detector inv-2x4 search-space tolerance 1)))

  
  (let [inv-4x4 [[0 0 0 0]
                 [0 0 0 0]
                 [1 0 0 0]
                 [0 0 0 1]]
        search-space [[0 1 0 0]
                      [0 1 0 0]
                      [0 1 0 1]
                      [1 0 1 0]
                      [0 0 0 0]]
        tolerance 0]
    (concat (invaders->matches [inv-4x4] search-space tolerance)
            (invader's-right-edge-detector  inv-4x4 search-space tolerance 1)
            (invader's-left-edge-detector   inv-4x4 search-space tolerance 1)
            (invader's-top-edge-detector    inv-4x4 search-space tolerance 1)
            (invader's-bottom-edge-detector inv-4x4 search-space tolerance 1)))
  
 )
