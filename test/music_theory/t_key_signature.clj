(ns music-theory.t-key-signature
	(:use midje.sweet)
	(:use music-theory.key-signature))

(facts "about `sharps-in-signature`"
			 (fact "Returns a map from a tonal-pitch-class to the number of sharps of that tonal-pitch-class
						 in the key signature for the diatonic collection starting on diatonic-root."
						 (sharps-in-signature :C) => (hash-map)
						 (sharps-in-signature :G) => (hash-map :F 1)
						 (sharps-in-signature :D) => (hash-map :F 1 :C 1)
						 (sharps-in-signature :C#) => (hash-map :F 1 :C 1 :G 1 :D 1 :A 1 :E 1 :B 1)
						 (sharps-in-signature :G#) => (hash-map :F 2 :C 1 :G 1 :D 1 :A 1 :E 1 :B 1)
						 (sharps-in-signature :D#) => (hash-map :F 2 :C 2 :G 1 :D 1 :A 1 :E 1 :B 1)
						 (sharps-in-signature :B#) => (hash-map :F 2 :C 2 :G 2 :D 2 :A 2 :E 1 :B 1)
						 ))

(facts "about `flats-in-signature`"
			 (fact "Returns a map from a tonal-pitch-class to the number of flats of that tonal-pitch-class
						 in the key signature for the diatonic collection starting on diatonic-root."
						 (flats-in-signature :C) => (hash-map)
						 (flats-in-signature :F) => (hash-map :B 1)
						 (flats-in-signature :Bb) => (hash-map :B 1 :E 1)
						 (flats-in-signature :Fb) => (hash-map :B 2 :E 1 :A 1 :D 1 :G 1 :C 1 :F 1)
						 ))
