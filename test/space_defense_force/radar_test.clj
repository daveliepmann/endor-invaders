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


(deftest invader-detection-api
  (testing "can find possible invaders in samples?"
    (let [scan-result (invaders->matches [invader1 invader2]
                                         radar-sample
                                         9)]
      (is (= 4 (count scan-result)))
      (is (every? (every-pred :coords :known-shape :match :tolerance :noise-count :score)
                  scan-result))
      (is (= #{[60 13] [16 28] [82 41] [42 0]}
             (set (map :coords scan-result)))))))
