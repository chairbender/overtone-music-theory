;For dealing with intervals
(ns music-theory.interval
  (:require [clojure.math.numeric-tower :as math]))


;The number of semitones above which intervals will be treated as compound intervals.
(def ^{:private true} compound-interval-cutoff 16)


(defn- interval-distance
  "Returns the absolute distance in semitones between two midi notes"
  [note otherNote]
  (math/abs (- (midi->semitones note) (midi->semitones otherNote))))

(defn- lower-note
	"Returns the keyword of the note that is lower.
	Both arguments should be note keywords in the same format as overtone's.
	If both are equal, returns note."
	[note other-note]
	(if (> (midi->semitones (note note)) (midi->semitones (note other-note)))
		(other-note)
		(note)
		))

(defn- higher-note
	"Returns the keyword of the note that is higher.
	Both arguments should be note keywords in the same format as overtone's.
	If both are equal, returns note."
	[note other-note]
	(if (>= (midi->semitones (note note)) (midi->semitones (note other-note)))
		(note)
		(other-note)
		))

(defn- note-interval-index
	"Given a note, returns what we will call its 'interval index'.
	For positive octaves and the zeroth octave, the interval-index of a note is defined to be the sum of
	the note letter's distance from A, plus 1 (So A, B, and C would be 1, 2, and 3 respectively)
	plus 8 * the note's octave.
	For negative octaves, it is defined as such: Take the negative of the note letter's distance from G, minus one
	(so A, B, and C would be -7, -6, and -5, with G being -1), plus 8 * (the note's octave + 1) (so the
	octave of -2 would be -16). That number + 1 is the interval index.
	This gives us an index that orders each possible note by its note letter and octave."
	[note]
	(if (< (note-octave note) 0)
	(inc (+ (dec (- (int (note-letter note)) (int \G)))
		 (* 8 (inc(note-octave note)))))
	(+ (inc (- (int (note-letter note)) (int \A)))
		 (* 8 (note-octave note)))))

(defn- interval-number
	"Given two notes in the note keyword format (same as overtone's),
	return the distance between the letters of the notes + 1, but considering
	the octave.
	For example, :A2 :A4 would return 16."
	[note other-note]
	;Figure out the higher and lower note
	(let [bottom-note (lower-note note other-note)
				top-note (higher-note note other-note)]
		(- (note-interval-index top-note) (note-interval-index bottom-note))
		))
;TODO - implement using stackoverflow answer
(defn interval-keyword
  "Returns the interval name given two note keywords"
  [note other-note]
  (if (<= (interval-distance note otherNote) compound-interval-cutoff)
    ;Interval is not compound
    (vector (get intervals-by-enharmonic (interval-distance note otherNote)))
    ;interval is compound
    (loop [resultVector []
           semitonesLeft (interval-distance note otherNote)]
      (if (<= semitonesLeft 12)
        (conj resultVector (get intervals-by-enharmonic semitonesLeft))
        (recur (conj resultVector (get intervals-by-enharmonic 12)) (- semitonesLeft 12))))))


(defn note-interval-above
	"Returns the midi note that is the given interval above the given note.
	 If multiple intervals are given, combines them. Intervals must be :maj10 or smaller."
	[note & interval]
	(apply note-interval (flatten [note true interval])))

(defn note-interval-below
	"Returns the midi note that is the given interval below the given note.
	 If multiple intervals are given, combines them. Intervals must be :maj10 or smaller."
	[note & interval]
	(apply note-interval (flatten [note false interval])))
