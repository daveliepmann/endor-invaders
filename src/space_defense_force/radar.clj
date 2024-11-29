(ns space-defense-force.radar
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))


(def known-invaders
  #{(slurp (io/resource "samples/invader1"))
    (slurp (io/resource "samples/invader2"))})


(def radar-sample
  (str/split-lines (slurp (io/resource "samples/radar1"))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(comment ;;;; Let's solve an easier problem first: exact match, one line at a time
  
  (def dev:samples
    (into #{}
          (map (comp first str/split-lines))
          known-invaders))

  (->> known-invaders
       (mapcat str/split-lines)
       (map count)
       distinct)
  ;; => (11 8)

  ;; minimum viable detection (exact match 1 at a time)

  (let [invdr (first dev:samples) ; "---oo---"
        len (count invdr)]
    (some #{invdr}
          (map #(apply str %)
               (partition len 1 (nth radar-sample 4)))))

  ;; is the next line where we'd expect it?
  (let [invdr2 (-> known-invaders first str/split-lines second) ; "---o---o---"
        ;; FIXME above line indicates `dev:samples` was wrong design
        len (count invdr2)]
    (some #{invdr2}
          (map #(apply str %)
               (partition len 1 (nth radar-sample 5)))))
  ;; hmm, no, but manual search shows that a 1-char change _would_ be a match
  
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Fuzzy matching

(defn fuzzy-match
  "Looks for `pattern` in `signal` TODO with `tolerance` num deviations"
  [pattern tolerance signal]
  (str/includes? signal pattern))
