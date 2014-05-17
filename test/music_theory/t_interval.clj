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
    (interval-keyword :A4 :G3) => :M2))
