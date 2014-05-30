;functions for doing counterpoint using tonal theory methods

(ns music-theory.tonal-theory.algorithmic-composition.counterpoint
  (:use music-theory.tonal-theory.line)
  (:use music-theory.tonal-theory.interval)
  (:use music-theory.tonal-theory.key)
  (:use music-theory.tonal-theory.note)
  (:use music-theory.tonal-theory.tonal-pitch-class))

(defn- note-letter-difference
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
      (* direction (+ (* octaves 7) interval-num)))))

(defn- full-diatonic-step-motion
  "For a given key, starting note, and ending note,
  returns a line consisting of whole notes where the notes form a step motion
  using the diatonic degrees of the key from the starting note to the ending note
  (including the start and end note). both notes must
  have pitch classes that are the key's diatonic degrees (no accidentals)"
  [key starting-note ending-note]
  (let [starting-index (.indexOf key (tonal-pitch-class-from-note starting-note))
        direction (if (>= 0 (note-letter-difference ending-note starting-note)) -1 1)
        starting-octave (note-octave starting-note)
        ;used to figure out when octaves are being crossed,
        ;as an offset from the key vector index (so in the key of C, the note C has a
        ;a key vector index of 0, but it is actually 2 notes above a, so key-index + a-offset should be 2)
        a-offset (- (int (tonal-pitch-class-letter (get key 0))) (int \A))]
    (let [ending-index (+ starting-index (note-letter-difference ending-note starting-note))]
      (apply line
             (flatten
               (for [i (range starting-index ending-index direction)]
                 (let [cur-tonal-pitch-class (get key (mod i 7))
                       cur-octave-offset (if (< (+ i a-offset) 0)
                                           (dec (quot (+ i a-offset) 7))
                                           (quot (+ i a-offset) 7))]
                   [
                     (note-from-tonal-pitch-class cur-tonal-pitch-class
                                                  (+ starting-octave cur-octave-offset))
                     1]
                   )))))))

(defn diatonic-step-motion
  "For a given key, starting note, and ending note,
  returns a line consisting of whole notes where the notes form a step motion
  using the diatonic degrees of the key from the starting note to the ending note
  (excluding the start and end note). both notes must
  have pitch classes that are the key's diatonic degrees (no accidentals)"
  [key starting-note ending-note]
  (let [full-step-motion (full-diatonic-step-motion key starting-note ending-note)]
    (subline full-step-motion 1 (dec (line-note-count full-step-motion)))))


;
;(defn basic-step-motion
;  "Given a scale (use the scale function in key.clj),
;  returns a line that follows the operational rules:
;
;  1. The final pitch in the basic step motion must be a tonic.
;  2. The first pitch must be a tonic triad member a thrid fifth or octave
;  above the final pitch.
;  3. These two pitches must be joined by inserting the pitches of intervening diatonic degrees to form
;  a descending step motion.
;
;  scale is the scale to use (a vector of 7 notes comprising the scale). first-note-interval-number
;  is either 3, 5, or 8 (representing the first note being a 3rd, fifth, or octave above the tonic)."
;  [scale first-note-interval-number]
;  (+ 1 1))


