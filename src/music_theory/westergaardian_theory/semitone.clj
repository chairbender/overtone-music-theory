
(ns
    ^{:doc "Operations involving semitones"
      :author "Kyle Hipke"}
    music-theory.westergaardian-theory.semitone
	(:require [clojure.math.numeric-tower :as math]))

(defn semitones->midi
  "Converts a value representing the number of semitones from C-1 to a midi note value."
  [semitones]
  semitones)
