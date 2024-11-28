(ns space-defense-force.radar
  (:require [clojure.java.io :as io]))

(def known-invaders
  #{(slurp (io/resource "samples/invader1"))
    (slurp (io/resource "samples/invader2"))})


(def radar-sample
  (slurp (io/resource "samples/radar1")))

