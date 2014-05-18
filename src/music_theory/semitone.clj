;Operations involving semitones
(ns music-theory.semitone
	(:require [clojure.math.numeric-tower :as math]))

(defn semitones->midi
  "Converts a value representing the number of semitones from C-1 to a midi note value."
  [semitones]
  (if (contains? #{0 1 2} (mod semitones 12))
    (+ semitones 9)
    (- semitones 3)))
