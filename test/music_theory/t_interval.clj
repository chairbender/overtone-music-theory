(ns music-theory.t-interval
  (:use midje.sweet)
  (:use [music-theory.interval]))

(facts "about `interval-keyword`"
       (fact "For all intervals it returns a keyword indicating
        the interval name, even for arbitrarily large intervals.
        So, for intervals larger than an octave, returns numbers like :P11"
             (interval-keyword :A4 :B4) => :M2
             (interval-keyword :A4 :C4) => :m3
             (interval-keyword :A3 :A3) => :P1
             (interval-keyword :A3 :A4) => :P8
             (interval-keyword :A3 :C#4) => :M10
             (interval-keyword :E#4 :Fb4) => :dd2
             (interval-keyword :G3 :A4) => :M2
             (interval-keyword :A4 :G3) => :M2
             (interval-keyword :C#4 :C5) => :d8
             (interval-keyword :C4 :C#5) => :a8))

(facts "about `interval-above`"
       (fact "Returns the note that is the given interval
       above the given note, with the correct enharmonic name."
             (interval-above :A4 :M2) => :B4
             (interval-above :A4 :m3) => :C4
             (interval-above :A3 :P8) => :A4
             (interval-above :A3 :P1) => :A3
             (interval-above :A3 :M10) => :C#4
             (interval-above :E#4 :dd2) => :Fb4
             (interval-above :G3 :M2) => :A4
             (interval-above :C#4 :d8) => :C5
             (interval-above :C4 :a8) => :C#5
             ))

(facts "about `interval-below`"
       (fact "Returns the note that is the given interval
       below the given note, with the correct enharmonic name."
             (interval-below :B4 :M2) => :A4
             (interval-below :C4 :m3) => :A4
             (interval-below :A4 :P8) => :A3
             (interval-below :A3 :P1) => :A3
             (interval-below :C#4 :M10) => :A3
             (interval-below :Fb4 :dd2) => :E#4
             (interval-below :A4 :M2) => :G3
             (interval-below :C5 :d8) => :C#4
             (interval-below :C#5 :a8) => :C4
             ))
