(ns music-theory.tonal-theory.algorithmic-composition.t-counterpoint
  (:use midje.sweet)
  (:use music-theory.tonal-theory.algorithmic-composition.counterpoint)
  (:use music-theory.tonal-theory.line)
  (:use music-theory.tonal-theory.key))

(facts "about `diatonic-step-motion`"
       (fact "returns a diatonic step motion line of whole notes
       between the two notes, using the degrees of the key"
             (diatonic-step-motion
               (key-vector :C :major)
               :C4
               :B5) => (line
                         :D4 1
                         :E4 1
                         :F4 1
                         :G4 1
                         :A5 1)

              (diatonic-step-motion
                (key-vector :A :minor)
                :B5
                :C4) => (line
                          :A5 1
                          :G4 1
                          :F4 1
                          :E4 1
                          :D4 1)))
