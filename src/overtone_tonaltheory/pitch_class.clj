;Stuff involving pitch classes, since clojure doesn't really
;care seem to provide much involving pitch-classes, and anyway
;it

(ns overtone-tonaltheory.pitch-class
	(:use clojure.set))

(def ^{:private true} circle-of-flats [:B :E :A :D :G :C :F])
(def ^{:private true} circle-of-sharps [:F :C :G :D :A :E :B])
(def ^{:private true :doc "Map from one natural pitch-class to the next natural pitch class"}
	next-natural-pitch-class-map (hash-map :A :B :B :C :C :D :D :E :E :F :F :G :G :A))
(def ^{:private true :doc "Map from one natural pitch-class to the previous natural pitch class"}
	previous-natural-pitch-class-map (map-invert next-natural-pitch-class-map))

(defn nth-sharp-in-key-signature
	"Returns the sharp that would appear as the nth sharp in a key signature, where
	0 means the first sharp.
	N must be 13 or less."
	[n]
	(if (>= n (count circle-of-sharps))
		(get circle-of-sharps (mod n (count circle-of-sharps)))
		(get circle-of-sharps n)))

(defn natural
	"Returns the passed keyword but with any accidental removed."
	[pitch-class]
	(keyword (str (first (name pitch-class)))))


(defn next-natural-pitch-class
	"Returns the 'natural' pitch class that is one higher
	than the given pitch class. 'Natural' here means without an accidental.
	So, if :Bb is given, the next natural will be :C. If B# is given, the next natural will
	be :C"
	[pitch-class]
	(next-natural-pitch-class-map (natural pitch-class)))

(defn previous-natural-pitch-class
	"Returns the 'natural' pitch class that is one lower
	than the given pitch class. 'Natural' here means without an accidental.
	So, if :Bb is given, the previous natural will be :A. If B# is given, the previous natural will
	be :A"
	[pitch-class]
	(previous-natural-pitch-class-map (natural pitch-class)))
