;Handles stuff involving key signatures -more focusing on the accidentals in the signature than the notes
;in the signatures. see key.clj for stuff that's more focused on the notes and modes and things like that
(ns music-theory.tonal-theory.key-signature
	(:use music-theory.tonal-theory.tonal-pitch-class))

(def ^{:private true} circle-of-flats [:B :E :A :D :G :C :F])
(def ^{:private true} circle-of-sharps [:F :C :G :D :A :E :B])

(defn nth-sharp-in-key-signature
  "Returns the sharp that would appear as the nth sharp in a key signature, where
	0 means the first sharp."
  [n]
  (if (>= n (count circle-of-sharps))
    (get circle-of-sharps (mod n (count circle-of-sharps)))
    (get circle-of-sharps n)))

(defn nth-flat-in-key-signature
  "Returns the sharp that would appear as the nth flat in a key signature, where
  0 means the first sharp."
  [n]
  (if (>= n (count circle-of-flats))
    (get circle-of-flats (mod n (count circle-of-flats)))
    (get circle-of-flats n)))

(def ^{:private true} previous-flat-in-circle-map
  (hash-map :F :C :C :G :G :D :D :A :A :E :E :B :B :F))

(defn previous-flat-in-circle
  "Returns the tonal-pitch-class of the flat that comes before the given
  flat in the circle of flats.
  tonal-pitch-class should be a pitch class keyword with no alteration"
  [tonal-pitch-class]
  (previous-flat-in-circle-map tonal-pitch-class))


(defn- last-sharp?
	"Figure out if we are at the last sharp in the key signature
	for the diatonic collection starting on diatonic-root.
	If the key signature tonal-pitch-class has n sharp alterations on it, check if the natural of that pitch class
	is already in the result-map with a value of (n) AND if the current sharp is a full step below the natural of the key
	sig pitch class. If so, we're done."
	[diatonic-root current-sharp current-key-accidentals]
	(if (>  (sharps diatonic-root) 0)
		(and (= (current-key-accidentals (natural diatonic-root)) (sharps diatonic-root))
				 (= (previous-natural-tonal-pitch-class (natural diatonic-root)) (natural current-sharp)))
		(= (previous-natural-tonal-pitch-class diatonic-root) current-sharp)
		))

(defn- last-flat?
	"Figure out if we are at the last flat in the key signature
	for the diatonic collection starting on diatonic-root.
	If the key signature is F, just check if the current flat
	is B. If it is, then that's the last one.
	If the key signature tonal-pitch-class has n flat alterations in it, check if the natural of that tonal-pitch-class
	is already in the result-map with a value of n AND if the current flat is
	the one that comes after the key signature natural pitch in the circle of flats.
	If so, we're done."
	[diatonic-root current-flat current-key-accidentals]
	(if (= diatonic-root :F)
		(= current-flat :B)
		(and (= (natural diatonic-root) (previous-flat-in-circle current-flat))
				 (= (current-key-accidentals (natural diatonic-root)) (flats diatonic-root)))
		))


(defn flats-in-signature
	"Returns a map from a tonal-pitch-class to the number of flats of that tonal-pitch-class in the
	key-signature for the diatonic collection formed from
	the given diatonic-root. (You can think of a diatonic collection starting
	on diatonic-root as equivalent to the key of <diatonic-root> major).
	For example the diatonic collection starting on F flat has a double flat for B, so B->2 in that map..
	undefined if diatonic root is not :C, :F, or does not have a flat alteration in it.
	"
	[diatonic-root]
	(if (tonal-pitch-class-equals diatonic-root :C)
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
	"Returns a map from a tonal-pitch-class to the number of sharps of that tonal-pitch-class in the
	key-signature for the diatonic collection formed from
	the given diatonic-root. (You can think of a diatonic collection starting
	on diatonic-root as equivalent to the key of <diatonic-root> major).
	For example the diatonic collection starting on G sharp has a double sharp for F, so F->2 in that map.
	diatonic-root should be a tonal-pitch-class keyword (like :A :B or :G#).
	undefined if diatonic root has a flat alteration in it.
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
