(ns overtone-tonaltheory.t-interval
  (:use midje.sweet)
  (:use [overtone-tonaltheory.interval])
  (:use overtone.core))

(facts "about `interval-keywords`"
  (fact "For intervals up to a major tenth, it returns a vector of a single set
        that contains the keywords describing those intervals"
    (interval-keywords :A4 :B4) => [#{:maj2}]
    (interval-keywords :A4 :C4) => [#{:m3}]
    (interval-keywords :A3 :A3) => [#{:unison}]
    (interval-keywords :A3 :A4) => [#{:octave}]
    (interval-keywords :A3 :C#4) => [#{:maj10}]
    (interval-keywords :D4 :F#4) => [#{:maj3 :dim4}])

  (fact "For intervals greater than a major tenth, it returns a vector
        of the intervals making up the compound interval, with octaves
        being the highest interval possible and comingfirst"
    (interval-keywords :A3 :D4) => [#{:octave} #{:4}]
    (interval-keywords :A3 :A5) => [#{:octave} #{:octave}])
    (interval-keywords :D3 :E5) => [#{:octave} #{:octave} #{:maj2}])


(facts "about `note-interval-above'"
  (fact "Returns the midi note that is the given interval above the given note.
				If multiple intervals are given, combines them"
    (note-interval-above :A4 :unison) => (note :A4)
    (note-interval-above :A4 :octave) => (note :A5)
    (note-interval-above :D4 :octave :m2) => (note :D#5)
		(note-interval-above :D4 :octave :m2 :m2) => (note :E5)
		(note-interval-above :C4 :5) => (note :G4)))

(facts "about `note-interval-below'"
  (fact "Returns the midi note that is the given interval below the given note.
				If multiple intervals are given, combines them"
    (note-interval-below :A4 :unison) => (note :A4)
    (note-interval-below :A4 :octave) => (note :A3)
    (note-interval-below :D4 :octave :m2) => (note :Db3)
		(note-interval-below :D4 :octave :m2 :m2) => (note :C3)
		(note-interval-below :C4 :5) => (note :F3)))


