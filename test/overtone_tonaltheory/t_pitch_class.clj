(ns overtone-tonaltheory.t-pitch-class
	(:use midje.sweet)
	(:use overtone-tonaltheory.pitch-class))

(facts "about `nth-sharp-in-key-signature`"
			 (fact "Returns the sharp that would appear as the nth sharp in a key signature
						 (0 being the first), if n is 13 or less"
						 (nth-sharp-in-key-signature 0) => :F
						 (nth-sharp-in-key-signature 1) => :C
						 (nth-sharp-in-key-signature 6) => :B
						 (nth-sharp-in-key-signature 7) => :F
						 (nth-sharp-in-key-signature 13) => :B
						 ))
