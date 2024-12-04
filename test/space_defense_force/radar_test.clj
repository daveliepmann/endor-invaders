(ns space-defense-force.radar-test
  (:require [space-defense-force.radar :refer :all]
            [clojure.test :refer :all]
            [clojure.java.io :as io]))

(def radar-sample
  (->binary-vec (slurp (io/resource "samples/radar1"))))

(def invader1 (->binary-vec (slurp (io/resource "samples/invader1"))))

(def invader2 (->binary-vec (slurp (io/resource "samples/invader2"))))


(deftest dimensions
  (testing "can get dimensions of vec-of-vecs?"
    (is (thrown? ; TODO find an alternative to this magic
         clojure.lang.ExceptionInfo
         (dims '(:foo))))
    (is [2 2] (dims [[1 1] [1 1]]))
    (is [11 8] (dims invader1))
    (is [8 8] (dims invader2))
    (is [100 50] (dims radar-sample))))


(deftest signal-formats
  (testing "can roundtrip signal formats?"
    (is (= invader1 (->binary-vec (->signal-str invader1))))
    (is (= invader2 (->binary-vec (->signal-str invader2))))))


(deftest permutations
  (testing "permutations give sub-rects?"
    (is (= {[0 0] [[0]],
            [1 0] [[0]],
            [0 1] [[0]],
            [1 1] [[1]]}
           (permutations-by-xy [1 1]
                               [[0 0]
                                [0 1]])))
    (is (= {[0 0] [[1 0] [0 1]],
            [1 0] [[0 1] [1 0]],
            [0 1] [[0 1] [1 0]],
            [1 1] [[1 0] [0 1]]}
           (permutations-by-xy [2 2]
                               [[1 0 1]
                                [0 1 0]
                                [1 0 1]])))))


(deftest matches
  (testing "can exact match?"
    (let [rect-2x2 [[1 1]
                    [1 0]]
          tautological-2x2 (fuzzy-match rect-2x2 rect-2x2 0)]
      (is (= (:known-shape tautological-2x2)
             (:match tautological-2x2)
             rect-2x2)))
    (let [tautological-invader (fuzzy-match invader1 invader1 0)]
      (is (= (:known-shape tautological-invader)
             (:match tautological-invader)
             invader1))))
  (testing "can fuzzy match?"
    (let [rect1 [[1 1]
                 [1 0]]
          rect2 [[1 1]
                 [1 1]]
          match-tolerance0 (fuzzy-match rect1 rect2 0)
          match-tolerance1 (fuzzy-match rect1 rect2 1)]
      (is (empty? match-tolerance0))
      (is (some? match-tolerance1))
      (is (= rect1 (:known-shape match-tolerance1)))
      (is (= rect2 (:match match-tolerance1)))))
  (testing "can fuzzy match invader1?"
    (let [invader1' (-> invader1 ; change the upper-left & bottom-right corners
                        (assoc-in [0 0] 1)
                        (assoc-in [7 10] 1))]
      (is (empty? (fuzzy-match invader1 invader1' 0)))
      (is (empty? (fuzzy-match invader1 invader1' 1)))
      (is (= invader1' (:match (fuzzy-match invader1 invader1' 2)))))))


(def match?
  (every-pred :coords :known-shape :match :tolerance :noise-count :score))

(def unscored-match?
  (every-pred :coords :known-shape :match :tolerance :noise-count))

(deftest invader-detection-api
  (testing "can find possible invaders in samples?"
    (let [scan-result (invaders->matches [invader1 invader2]
                                         radar-sample
                                         9)]
      (is (= 4 (count scan-result)))
      (is (every? match? scan-result))
      (is (= #{[60 13] [16 28] [82 41] [42 0]}
             (set (map :coords scan-result)))))))

;; TODO more details for below testing of edge-case fns `invader's-right-edge-detector` usw.

(deftest edge:invader|space
  (testing "invader right edge (space left edge)"
    (testing "minimum example: exact match of single col"
      (let [match-1x3 (invader's-right-edge-detector [[0 1 1]
                                                      [0 1 1]
                                                      [0 0 1]]
                                                     [[1 0 1]
                                                      [1 0 1]
                                                      [1 0 0]]
                                                     0 1)]
        (is (= 1 (count match-1x3)))
        (is (unscored-match? (first match-1x3)))
        (is (= [[1] [1] [1]]
               (:known-shape (first match-1x3))
               (:match (first match-1x3))))))
    (testing "mismatch height"
      (let [match-1x2 (invader's-right-edge-detector [[0 1 1]
                                                      [0 1 1]]
                                                     [[0 0 0]
                                                      [1 0 0]
                                                      [1 0 0]]
                                                     0 1)]
        (is (= 1 (count match-1x2)))
        (is (unscored-match? (first match-1x2)))
        (is (= [[1] [1]]
               (:known-shape (first match-1x2))
               (:match (first match-1x2))))))  
    (testing "mismatch height w/more width"
      (let [match-2x2 (invader's-right-edge-detector [[0 1 1]
                                                      [0 1 1]]
                                                     [[0 0 0]
                                                      [1 1 0]
                                                      [0 1 0]]
                                                     1 2)]
        (is (= 1 (count match-2x2)))
        (is (unscored-match? (first match-2x2)))
        (is (= [[1 1] [1 1]]
               (:known-shape (first match-2x2))))
        (is (= [[1 1] [0 1]]
               (:match (first match-2x2))))))
    (testing "multiple matches"
      (let [matches-2x2 (invader's-right-edge-detector [[0 1 1]
                                                        [0 1 1]]
                                                       [[1 1 1]
                                                        [1 1 1]
                                                        [1 1 1]
                                                        [0 1 1]]
                                                       ;; note: only 2 matches with 0 tolerance
                                                       1 2)]
        (is (= 3 (count matches-2x2)))
        (is (every? unscored-match? matches-2x2))
        (is (= [[1 1] [1 1]]
               (:known-shape (first matches-2x2))
               (:match (first matches-2x2))))))))



(deftest edge:space|invader
  (testing "invader left edge (space right edge)"
    (testing "minimum example: exact match of single col"
      (let [match-1x3 (invader's-left-edge-detector [[0 1 1]
                                                     [0 1 1]
                                                     [0 1 1]]
                                                    [[1 1 0]
                                                     [1 1 0]
                                                     [1 1 0]]
                                                    0 1)]
        (is (= 1 (count match-1x3)))
        (is (unscored-match? (first match-1x3)))
        (is (= [[0] [0] [0]]
               (:known-shape (first match-1x3))
               (:match (first match-1x3))))))
    (testing "mismatch height"
      (let [match-1x2 (invader's-left-edge-detector [[1 1 1]
                                                     [1 1 1]]
                                                    [[0 0 0]
                                                     [0 0 1]
                                                     [0 0 1]]
                                                    0 1)]        
        (is (= 1 (count match-1x2)))
        (is (unscored-match? (first match-1x2)))
        (is (= [[1] [1]]
               (:known-shape (first match-1x2))
               (:match (first match-1x2))))))
    (testing "multiple matches"
      (let [matches-2x2 (invader's-left-edge-detector [[1 1 1]
                                                       [1 1 1]]
                                                      [[0 1 1]
                                                       [0 1 1]
                                                       [0 1 1]
                                                       [0 0 1]]
                                                      ;; note: only 2 matches with 0 tolerance
                                                      1 2)]
        (is (= 3 (count matches-2x2)))
        (is (every? unscored-match? matches-2x2))
        (is (= [[1 1] [1 1]]
               (:known-shape (first matches-2x2))
               (:match (first matches-2x2))))))))


(deftest edge:space>invader
  (testing "invader top edge (radar bottom edge)"
    (testing "minimum example: exact match of single row"
      (let [match-3x1 (invader's-top-edge-detector [[1 1 1]
                                                    [0 0 0]
                                                    [0 0 0]]
                                                   [[0 0 0]
                                                    [0 0 0]
                                                    [1 1 1]]
                                                   0 1)]
        (is (= 1 (count match-3x1)))
        (is (unscored-match? (first match-3x1)))
        (is (= [[1 1 1]]
               (:known-shape (first match-3x1))
               (:match (first match-3x1))))))
    (testing "mismatch width"
      (let [match-2x1 (invader's-top-edge-detector [[1 1]
                                                    [0 0]]
                                                   [[0 0 0]
                                                    [0 0 1]
                                                    [0 1 1]]
                                                   0 1)]
        (is (unscored-match? (first match-2x1)))
        (is (= [[1 1]]
               (:known-shape (first match-2x1))
               (:match (first match-2x1))))))
    (testing "multiple matches"
      (let [matches-2x1 (invader's-top-edge-detector
                         [[1 1]
                          [0 0]]
                         [[0 0 0 0]
                          [0 0 0 0]
                          [0 0 0 0]
                          [0 1 1 1]]
                         ;; note: only 2 matches with 0 tolerance
                         0 1)]
        (is (= 2 (count matches-2x1)))
        (is (every? unscored-match? matches-2x1))
        (is (= [[1 1]]
               (:known-shape (first matches-2x1))
               (:match (first matches-2x1))))))
    (testing "deeper height"
      (let [matches-2x2 (invader's-top-edge-detector
                         [[1 1]
                          [1 1]]
                         [[0 0 0 0]
                          [0 0 0 0]
                          [1 1 1 1]
                          [0 1 0 1]]
                         ;; note: 0 matches with 0 tolerance
                         1 #_0 2)]
        (is (= 3 (count matches-2x2)))
        (is (every? unscored-match? matches-2x2))))))


(deftest edge:invader>space
  (testing "invader bottom edge (radar top edge)"
    (testing "minimum example: exact match of single row"
      (let [match-3x1 (invader's-bottom-edge-detector [[1 1 1]
                                                       [0 0 0]
                                                       [0 0 0]]
                                                      [[0 0 0]
                                                       [0 0 0]
                                                       [1 1 1]]
                                                      0 1)]
        (is (= 1 (count match-3x1)))
        (is (unscored-match? (first match-3x1)))
        (is (= [[0 0 0]]
               (:known-shape (first match-3x1))
               (:match (first match-3x1))))))
    (testing "mismatch width"
      (let [match-2x1 (invader's-bottom-edge-detector [[1 1]
                                                       [0 0]]
                                                      [[0 0 0]
                                                       [0 0 1]
                                                       [0 1 1]]
                                                      0 1)]
        (is (unscored-match? (first match-2x1)))
        (is (= [[0 0]]
               (:known-shape (first match-2x1))
               (:match (first match-2x1))))))
    (testing "multiple matches"
      (let [matches-2x1 (invader's-bottom-edge-detector
                         [[1 1]
                          [0 0]]
                         [[0 0 0 0]
                          [0 0 0 0]
                          [0 0 0 0]
                          [0 1 1 1]]
                         ;; note: only 2 matches with 0 tolerance
                         0 1)]
        (is (= 3 (count matches-2x1)))
        (is (every? unscored-match? matches-2x1))
        (is (= [[0 0]]
               (:known-shape (first matches-2x1))
               (:match (first matches-2x1))))))
    (testing "deeper height"
      (let [matches-2x2 (invader's-bottom-edge-detector [[0 0]
                                                         [1 1]
                                                         [1 1]]
                                                        [[1 1 1 1]
                                                         [1 1 0 1]
                                                         [0 0 0 0]
                                                         [0 0 0 0]]
                                                        ;; note: 1 match with 0 tolerance
                                                        1 2)]
        (is (= 3 (count matches-2x2)))
        (is (every? unscored-match? matches-2x2))))))
