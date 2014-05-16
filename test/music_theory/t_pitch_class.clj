(ns music-theory.t-pitch-class
	(:use midje.sweet)
	(:use music-theory.pitch-class))

(facts "about `nth-sharp-in-key-signature`"
			 (fact "Returns the sharp that would appear as the nth sharp in a key signature"
						 (nth-sharp-in-key-signature 0) => :F
						 (nth-sharp-in-key-signature 1) => :C
						 (nth-sharp-in-key-signature 6) => :B
						 (nth-sharp-in-key-signature 7) => :F
						 (nth-sharp-in-key-signature 13) => :B
						 ))
