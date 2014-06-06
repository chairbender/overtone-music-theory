(ns music-theory.westergaardian-theory.algorithmic-composition.t-counterpoint
  (:use midje.sweet)
  (:use music-theory.westergaardian-theory.algorithmic-composition.counterpoint)
  (:use music-theory.westergaardian-theory.line)
  (:use music-theory.westergaardian-theory.key))

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
                          :D4 1)
              (diatonic-step-motion
                (key-vector :C :major)
                :C5 :C4) => (line
                              :B5 1
                              :A5 1
                              :G4 1
                              :F4 1
                              :E4 1
                              :D4 1)

              ))

(facts "about `basic-step-motion`"
       (fact "returns the basic step motion of counterpoint in westergaardian theory"
             (basic-step-motion (key-vector :C :major)
                                4
                                3) => (line
                                         :E4 1
                                         :D4 1
                                         :C4 1)

             (basic-step-motion (key-vector :A :minor)
                                4
                                3) => (line
                                        :C4 1
                                        :B4 1
                                        :A4 1)

             (basic-step-motion (key-vector :C :major)
                                4
                                8) => (line
                                        :C5 1
                                        :B5 1
                                        :A5 1
                                        :G4 1
                                        :F4 1
                                        :E4 1
                                        :D4 1
                                        :C4 1)

             ))
