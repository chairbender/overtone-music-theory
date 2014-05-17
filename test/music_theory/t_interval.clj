(ns music-theory.t-interval
  (:use midje.sweet)
  (:use [music-theory.interval]))

(facts "about `interval-keyword`"
  (fact "For all intervals it returns a keyword indicating
				the interval name, even for arbitrarily large intervals"
    (interval-keyword :A4 :B4) => :maj2
    (interval-keyword :A4 :C4) => :m3
    (interval-keyword :A3 :A3) => :unison
    (interval-keyword :A3 :A4) => :octave
    (interval-keyword :A3 :C#4) => :maj10))

(facts "about `compound-interval-keyword`"
       (fact "For all intervals it returns a keyword indicating the name
       of the compound interval. If the interval is not compound, just returns the interval name"
             (compound-interval-keyword :A4 :B4) => [:maj2]
             (compound-interval-keyword :A4 :C4) => [:m3]
             (compound-interval-keyword :A3 :A3) => [:unison]
             (compound-interval-keyword :A3 :A4) => [:octave])
       (fact "for intervals larger than an octave, it returns multiple intervals, with
       some number of octaves + an interval smaller than an octave to make up the interval"))

(facts "about `compound-interval-octaves`"
       (fact "For all intervals it returns the number of octaves in the compound interval.
       If the interval is not compound, returns 0."
             (interval-keywords-compound :A4 :B4) => [:maj2]
             (interval-keywords-compound :A4 :C4) => [:m3]
             (interval-keywords-compound :A3 :A3) => [:unison]
             (interval-keywords-compound :A3 :A4) => [:octave])
       (fact "for intervals larger than an octave, it returns multiple intervals, with
       some number of octaves + an interval smaller than an octave to make up the interval"))
