(ns space-defense-force.radar-test
  (:require [space-defense-force.radar :refer :all]
            [clojure.test :refer :all]
            [clojure.java.io :as io]))

(def radar-sample
  (->binary-vec (slurp (io/resource "samples/radar1"))))

(def invader1 (->binary-vec (slurp (io/resource "samples/invader1"))))

(def invader2 (->binary-vec (slurp (io/resource "samples/invader2"))))


(deftest dimensions
  (testing "dimensions"
    (is [2 2] (dims [[1 1] [1 1]]))
    (is [11 8] (dims invader1))
    (is [8 8] (dims invader2))
    (is [100 50] (dims radar-sample))))
