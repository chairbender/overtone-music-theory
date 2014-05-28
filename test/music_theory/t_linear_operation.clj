(ns music-theory.t-linear-operation
  (:use midje.sweet)
  (:use music-theory.linear-operation)
  (:use music-theory.line))


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
