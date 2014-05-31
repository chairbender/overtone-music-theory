(ns music-theory.tonal-theory.tonal-utility
  (:use music-theory.tonal-theory.interval))


(defn note-letter-difference
  "Returns the difference between the note letters (first-note - second-note)
   (so it is negative if the second note is higher than the second note), considering octave.
  So A4 C4 is 2, A4 C5 is 10, C#5 Abb4 is -10."
  [first-note second-note]
  (let [interval-num (interval-number (compound-interval-keyword first-note second-note))
        direction (if (= second-note (higher-note first-note second-note)) -1 1)
        octaves (Math/abs (interval-octaves first-note second-note))]
    ;have to have different behavior when
    ;interval num is 8 because compound interval returns
    ;8 for the interval number instead of 1
    (if (= interval-num 8)
      (* direction octaves 7)
      (* direction (+ (* octaves 7) (dec interval-num))))))
