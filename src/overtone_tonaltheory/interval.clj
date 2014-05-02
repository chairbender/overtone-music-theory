;For dealing with intervals
(ns overtone-tonaltheory.interval
  (:require [clojure.math.numeric-tower :as math])
  (:require [overtone.core :as overtone]))

;A vector of sets of enharmonically equivalent intervals (represented as keywords) in order from smallest to largest, ranging
;from unison to major 10th. The array index of an interval keyword set is equivalent to the
;number of semitones that are spanned by the intervals in that set.
(def ^{:private true} intervals-by-enharmonic [#{:unison}
                #{:m2}
                #{:maj2}
                #{:m3}
                #{:maj3 :dim4}
                #{:4}
                #{:aug4 :dim5}
                #{:5}
                #{:aug5 :m6}
                #{:maj6 :dim7}
                #{:aug6 :m7}
                #{:maj7}
                #{:octave}
                #{:augoctave :m9}
                #{:maj9}
                #{:m10}
                #{:maj10}])

;A map of each interval to the number of semitones it contains
(def ^{:private true} intervals-to-semitones
	 (apply hash-map (flatten (for [i (range (count intervals-by-enharmonic))]
		 (let [intervals-vector (into [] (intervals-by-enharmonic i))]
			 (for [j (range (count intervals-vector))]
				 [(get intervals-vector j) i]
				 )
			 )))))


;The number of semitones above which intervals will be treated as compound intervals.
(def ^{:private true} compound-interval-cutoff 16)

(defn- midi->semitones
  "Converts a midi note to a value representing the number of semitones from C-1.
  For example :A-1 is 0 and :C-1 is 3."
  [note]
  (if (contains? #{:A :Bb :B} ((overtone/note-info note) :pitch-class))
    (- (overtone/note note) 9)
    (+ (overtone/note note) 3)))
(defn- semitones->midi
  "Converts a value representing the number of semitones from C-1 to a midi note value.
  Reverse of midi->semitones"
  [semitones]
  (if (contains? #{0 1 2} (mod semitones 12))
    (+ semitones 9)
    (- semitones 3)))

(defn- interval-distance
  "Returns the absolute distance in semitones between two midi notes"
  [note otherNote]
  (math/abs (- (midi->semitones note) (midi->semitones otherNote))))

(defn interval-keywords
  "Returns a vector containing sets of the keywords that represent the interval between the two notes.
  The two notes must be in midi note format.
  The individual sets consist of the enharmonically-equivalent keys for the interval (for example, a
  set containing major 3rd and diminished 4th), while the vector contains those interval-sets.
  If the octave is larger than a major 10th, the vector will contain the intervals that comprise
  that compound interval, ordered from greatest to least interval. In the case of compound intervals,
  only intervals of an octave or less will be used to describe the interval.

  For example, if the interval between the notes is an eleventh, [{:octave}, {:4}] will be returned
  (representing an octave and a fourth).

  The interval keywords follow a form similar to other overtone keywords:
  :# is a perfect interval (for example :5 is a perfect fifth)
  :m#, maj#, aug#, dim# represents minor, major, augmented, and diminished intervals
  :unison, octave, and augoctave represent unison, octave, and augmented octave intervals."
  [note otherNote]
  (if (<= (interval-distance note otherNote) compound-interval-cutoff)
    ;Interval is not compound
    (vector (get intervals-by-enharmonic (interval-distance note otherNote)))
    ;interval is compound
    (loop [resultVector []
           semitonesLeft (interval-distance note otherNote)]
      (if (<= semitonesLeft 12)
        (conj resultVector (get intervals-by-enharmonic semitonesLeft))
        (recur (conj resultVector (get intervals-by-enharmonic 12)) (- semitonesLeft 12))))))

(defn interval-keyword-scale
	"Returns the keyword representing the interval between the two notes")

(defn- interval-semitones
	"Returns the number of semitones in the given interval (keyword) spans."
	[interval]
	(intervals-to-semitones interval))

(defn- note-interval
	"Returns the midi note that is the given interval away from the midi note.
	If multiple intervals given, combines them.
	The parameter above? determines whether the returned note should be above or below the given note."
	[note above? & intervals]
	(if above?
		(semitones->midi (+ (midi->semitones note) (reduce + (map #(interval-semitones %) intervals))))
		(semitones->midi (- (midi->semitones note) (reduce + (map #(interval-semitones %) intervals))))))


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
