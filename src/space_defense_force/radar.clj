(ns space-defense-force.radar
  "Invader-detection API

  Fns to reveal possible locations of known hostiles in ASCII radar
  signals. Representative signals are in `resources/samples`.

  Matches are currently implemented as maps with keys:
   `:known-shape` - input parameter describing possibly-detected invader rectangle
   `:tolerance` - input parameter which allowed the fuzzy match
   `:match` - the rectangle in the signal which may match a known invader
   `:coords` - x/y coordinates of the match (optional at certain stages)

  API consumers: see 'Recommended public API' and `possible-invaders`"
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))


;; TODO edge cases


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Utilities

(defn dims
  "Returns width and height of rectangular vector of vectors `shape`"
  [shape]
  (when-not (and (vector? shape) (every? vector? shape))
    (throw (IllegalArgumentException. "dims not defined if shape is not vector of vectors")))
  (when-not (apply = (map count shape))
    (throw (IllegalArgumentException. "dims not defined for non-rectangular shapes")))
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
        ys (range 0 (count (first rect)))]
    (when (>= tolerance
              (->> (for [x xs, y ys] [x y])  ; all combos
                   (map (fn [[x y]] (= (get-in rect [x y])
                                      (get-in shape [x y]))))
                   (filter false?)
                   count))
      {:known-shape shape, :tolerance tolerance, :match rect})))


(comment ;;; How to use

  (fuzzy-match dbg:invader1 dbg:invader1 0) ; tautological case

  ;; very small rects can be visually verified
  (fuzzy-match [[1 1]
                [1 0]]
               [[1 1]
                [1 0]]
               0)

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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Recommended public API

(defn possible-invaders
  "Returns coordinates where "; FIXME
  [shape search-space tolerance]
  (let [xy (dims shape)]
    (keep (fn [[xy rect]] (when-let [m (fuzzy-match shape rect tolerance)]
                           (assoc m :coords xy)))
          (permutations-by-xy xy search-space))))

;; TODO API to handle multiple shapes

(comment
  (possible-invaders dbg:invader1 dbg:radar-sample 8)

  )
