;todo test
(ns overtone-tonaltheory.t-diatonic-collection
	(:use midje.sweet)
	(:use overtone-tonaltheory.diatonic-collection))

(facts "about `diatonic-collection`"
			 (fact "Returns a diatonic collection as a vector of the pitch classes making up the collection,
						 formed by starting on the given pitch class."
						 (diatonic-collection :C) => [:C :D :E :F :G :A :B]
						 (diatonic-collection :D) => [:D :E :F# :G :A :B :C#]
						 (diatonic-collection :A#) => [:A# :B# :C## :D# :E# :F## :G##]
						 (diatonic-collection :F) => [:F :G :A :Bb :C :D :E]
						 (diatonic-collection :Fb) => [:Fb :Gb :Ab :Bbb :Cb :Db :Eb]
						 (diatonic-collection :Ab) => [:Ab :Bb :C :Db :Eb :F :G]))
