;Handles stuff involving key signatures
(ns music-theory.key-signature
	(:use music-theory.pitch-class))

(defn- is-sharp
	"Returns whether the given pitch-class has a sharp in it."
	[pitch-class]
	(.endsWith (name pitch-class) "#"))

(defmacro dbg[x] `(let [x# ~x] (println "dbg:" '~x "=" x#) x#))

(defn- last-sharp?
	"Figure out if we are at the last sharp in the key signature
	for the diatonic collection starting on diatonic-root.
	If the key signature has no sharp in it, just check if the current sharp
	is a full step below the key sig pitch. If it is, then that's the last one.
	If the key signature has a sharp in it, check if the accidental with that sharp
	is already in the result-map AND if the current sharp is a full step below the natural key sig pitch. If so, we're done."
	[diatonic-root current-sharp current-key-accidentals]
	(if (is-sharp diatonic-root)
		(and (= (current-key-accidentals (natural diatonic-root)) 1)
				 (= (previous-natural-pitch-class (natural diatonic-root)) (natural current-sharp)))
		(= (previous-natural-pitch-class diatonic-root) current-sharp)
		))

(defn- last-flat?
	"Figure out if we are at the last flat in the key signature
	for the diatonic collection starting on diatonic-root.
	If the key signature is F, just check if the current flat
	is B. If it is, then that's the last one.
	If the key signature has a flat in it, check if the accidental with that flat
	is already in the result-map AND if the current flat is
	the one that comes after the key signature pitch in the circle of flats.
	If so, we're done."
	[diatonic-root current-flat current-key-accidentals]
	(if (= diatonic-root :F)
		(= current-flat :B)
		(and (= (natural diatonic-root) (previous-flat-in-circle current-flat))
				 (= (current-key-accidentals (natural diatonic-root)) 1))
		))


(defn flats-in-signature
	"Returns a map from a pitch-class to the number of flats of that pitch-class in the
	key-signature for the diatonic collection formed from
	the given diatonic-root. (You can think of a diatonic collection starting
	on diatonic-root as equivalent to the key of <diatonic-root> major).
	For example the diatonic collection starting on F flat has a double flat for B, so B->2 in that map.
	diatonic-root should be a pitch-class keyword that is either :F, :C, or any pitch class
	with exactly one single flat accidental (like : :Bb or :Gb).
	Undefined for anything that doesn't meet that.
	"
	[diatonic-root]
	(if (= diatonic-root :C)
		(hash-map)
		(loop [next-flat-index 0
					 result-map-vector []] ;using a vector to make it easier to handle counting frequency
			(let [next-flat (nth-flat-in-key-signature next-flat-index)
						result-map (frequencies result-map-vector)]

				(if (last-flat? diatonic-root next-flat result-map)
					(frequencies (conj result-map-vector next-flat))
					(recur (inc next-flat-index) (conj result-map-vector next-flat))
					)))))

(defn sharps-in-signature
	"Returns a map from a pitch-class to the number of sharps of that pitch-class in the
	key-signature for the diatonic collection formed from
	the given diatonic-root. (You can think of a diatonic collection starting
	on diatonic-root as equivalent to the key of <diatonic-root> major).
	For example the diatonic collection starting on G sharp has a double sharp for F, so F->2 in that map.
	diatonic-root should be a pitch-class keyword (like :A :B or :G#).
	Works correctly for a diatonic root of any pitch-class including sharped pitch-classes. Undefined
	for pitch-classes that contain flats or more than one accidental (like double sharps).
	"
	[diatonic-root]
	(if (= diatonic-root :C)
		(hash-map)
		(loop [next-sharp-index 0
					 result-map-vector []] ;using a vector to make it easier to handle counting frequency
			(let [next-sharp (nth-sharp-in-key-signature next-sharp-index)
						result-map (frequencies result-map-vector)]

				(if (last-sharp? diatonic-root next-sharp result-map)
					(frequencies (conj result-map-vector next-sharp))
					(recur (inc next-sharp-index) (conj result-map-vector next-sharp))
					)))))
