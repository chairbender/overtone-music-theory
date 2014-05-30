(ns music-theory.t-key
  (:use midje.sweet)
  (:use music-theory.key))


(facts "about `key-tonal-pitch-classes`"
       (fact "returns the tonal pitch classes of the key, with the tonic
       first."
             (key-tonal-pitch-classes :C :major) => [:C :D :E :F :G :A :B]

             (key-tonal-pitch-classes :A :minor) => [:A :B :C :D :E :F :G]

             (key-tonal-pitch-classes :C# :major) => [:C# :D# :E# :F# :G# :A# :B#]
             (key-tonal-pitch-classes :A# :minor) => [:A# :B# :C# :D# :E# :F# :G# ]

             ))

(facts "about `scale`"
       (fact "returns the notes of the scale starting on the given note"
             (scale :C4 :major) => [:C4 :D4 :E4 :F4 :G4 :A5 :B5]

             (scale :A4 :minor) => [:A4 :B4 :C4 :D4 :E4 :F4 :G4]

             (scale :C#4 :major) => [:C#4 :D#4 :E#4 :F#4 :G#4 :A#5 :B#5]
             (scale :A#4 :minor) => [:A#4 :B#4 :C#4 :D#4 :E#4 :F#4 :G#4 ]

             ))


