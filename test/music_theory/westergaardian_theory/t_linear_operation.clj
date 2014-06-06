(ns music-theory.westergaardian-theory.t-linear-operation
  (:use midje.sweet)
  (:use music-theory.westergaardian-theory.linear-operation)
  (:use music-theory.westergaardian-theory.line))


(facts "about `rearticulation`"
       (fact "Returns a line containing a rearticulation of the note at the given index."
             (rearticulation
               (line :A4 1)
               0 1/2) => (line :A4 1/2 :A4 1/2)

             (rearticulation
               (line :A4 1 :B4 1 :C4 1)
               0 1/2) => (line :A4 1/2 :A4 1/2 :B4 1 :C4 1)

             (rearticulation
               (line :A4 1 :B4 1 :C4 1)
               1 1/2) => (line :A4 1 :B4 1/2 :B4 1/2 :C4 1)

             (rearticulation
               (line :A4 1 :B4 1 :C4 1)
               2 1/2) => (line :A4 1 :B4 1 :C4 1/2 :C4 1/2)

             (rearticulation
               (line :A4 1 :B4 1/2 :C4 1)
               1 1/4) => (line :A4 1 :B4 1/4 :B4 1/4 :C4 1)

             (rearticulation
               (line :A4 1 :B4 1 :C4 1)
               1 1/3) => (line :A4 1 :B4 1/3 :B4 2/3 :C4 1)
             )
  )

(facts "about `neighbor`"
       (fact "Returns a line containing a neighbor of the articulation starting at the given index."
             (neighbor
               (line :C4 1
                     :D4 1
                     :D4 1
                     :E4 1)
               1 true :M2 1/2) => (line :C4 1
                                        :D4 1/2
                                        :E4 1/2
                                        :D4 1
                                        :E4 1)

             (neighbor
               (line :C4 1/2
                     :C4 1/2
                     :D4 1
                     :D4 1/4
                     :E4 1)
               0 false :m2 1/4) => (line :C4 1/4
                                         :B4 1/4
                                         :C4 1/2
                                         :D4 1
                                         :D4 1/4
                                         :E4 1)
             ))

(facts "about `arpeggiate`"
       (fact "arpeggiates the given note in the line"

             (arpeggiate
               (line :A4 1 :C4 1 :D4 1)
               1 true :E4 1/2) => (line :A4 1 :C4 1/2 :E4 1/2 :D4 1)

             (arpeggiate
               (line :A4 1 :C4 1 :D4 1)
               1 false :E4 1/2) => (line :A4 1 :E4 1/2 :C4 1/2 :D4 1)

             (arpeggiate
               (line :A4 1 :C4 1 :D4 1)
               1 true :G3 1/2) => (line :A4 1 :G3 1/2 :C4 1/2 :D4 1)

             (arpeggiate
               (line :A4 1 :C4 1 :D4 1)
               1 false :G3 1/2) => (line :A4 1 :C4 1/2 :G3 1/2 :D4 1)
             ))

(facts "about `step-motion`"
       (fact "inserts the step-motion line at the given index, borrowing from the duration of the note
       at that index."
             (step-motion
               (line :A4 1 :B4 1 :F4 1)
               1
               (line :C4 1/4 :D4 1/4 :E4 1/4)
               ) => (line :A4 1 :B4 1/4 :C4 1/4 :D4 1/4 :E4 1/4 :F4 1)

             (step-motion
               (line :A4 1 :E4 1 :F4 1)
               0
               (line :B4 1/4 :C4 1/4 :D4 1/4)
               ) => (line :A4 1/4 :B4 1/4 :C4 1/4 :D4 1/4 :E4 1 :F4 1)
             ))
