;Operations involving semitones
(ns overtone-tonaltheory.semitone
	(:require [clojure.math.numeric-tower :as math]))

(defn midi->semitones
  "Converts a midi note to a value representing the number of semitones from C-1.
  For example :A-1 is 0 and :C-1 is 3."
  [note]
  (if (contains? #{:A :Bb :B} ((overtone/note-info note) :pitch-class))
    (- (overtone/note note) 9)
    (+ (overtone/note note) 3)))
(defn semitones->midi
  "Converts a value representing the number of semitones from C-1 to a midi note value.
  Reverse of midi->semitones"
  [semitones]
  (if (contains? #{0 1 2} (mod semitones 12))
    (+ semitones 9)
    (- semitones 3)))

(defn semitone-interval
  "Returns the absolute distance in semitones between two midi notes"
  [note otherNote]
  (math/abs (- (midi->semitones note) (midi->semitones otherNote))))
