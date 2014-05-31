;functions for doing counterpoint using tonal theory methods

(ns music-theory.tonal-theory.algorithmic-composition.counterpoint
  (:use music-theory.tonal-theory.line)
  (:use music-theory.tonal-theory.interval)
  (:use music-theory.tonal-theory.key)
  (:use music-theory.tonal-theory.note)
  (:use music-theory.tonal-theory.tonal-pitch-class)
  (:use music-theory.tonal-theory.linear-operation)
  (:use music-theory.tonal-theory.tonal-utility)
  (:use music-theory.utility))

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
    (let [ending-index (+ starting-index (note-letter-difference ending-note starting-note) direction)]
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



(defn basic-step-motion
  "Given a key and starting octave (use the scale function in key.clj),
  returns a line that follows the operational rules:

  1. The final pitch in the basic step motion must be a tonic.
  2. The first pitch must be a tonic triad member a thrid fifth or octave
  above the final pitch.
  3. These two pitches must be joined by inserting the pitches of intervening diatonic degrees to form
  a descending step motion.

  key-vector is the key to use (a vector of 7 tonal pitch classes). octave is the octave
  to play the ending tonic on. first-note-interval-number
  is either 3, 5, or 8 (representing the first note being a major or minor 3rd (based on the key being major or minor), perfect fifth, or perfect octave above the tonic)."
  [key-vector octave first-note-interval-number]
  (let [scale (scale key-vector octave)]

      (let [starting-note (cond
                        (= first-note-interval-number 3)
                        (get scale 2)

                        (= first-note-interval-number 5)
                        (get scale 4)

                        (= first-note-interval-number 8)
                        (interval-above (get scale 0) :P8))
            ending-note (get scale 0)]
        (full-diatonic-step-motion key-vector starting-note ending-note))))

(defn- valid-neighbor-indexes
  "Returns an array containing the indexes of
  notes in the line that are repetitions of the same pitch (the index of the first note
  of the repetition). Durations are not considered when considering valid neighbor
  indexes."
  [target-line]
  (vec
    (filter
      #(not= % -1)
      (for [i (range (dec (line-note-count target-line)))]
        (if (= (:note (line-note-at target-line i)) (:note (line-note-at target-line (inc i))))
          i
          -1)))))

(defn random-neighbor
  "Given a line, performs a random neighbor operation on
   consecutive notes that have a repeated pitch within the counterpoint
  rules (the neighbor is always a member of the diatonic collection except
  when it is the lower neighbor to the tonic in a minor key, in which case it is altered
  so as to be a minor second from the tonic. The duration of the neighbor is chosen to always
  be half of the duration of the neighbored note
  Key-vector is the key vector of the key the line is in. target-line is the
  line to perform the operation on. Returns key-vector unmodified
   if there is no valid pitch repetition to neighbor."
  [key-vector target-line]
  (let
      [valid-indexes (valid-neighbor-indexes target-line)]

      (let [target-index (get valid-indexes (rand-int (line-note-count valid-indexes)))
        up? (rand-bool)]
    (let [target-line-note (line-note-at target-line target-index)
          direction (if up? 1 -1)]

      (if (nil? target-line-note)
        ;return target line if no valid indexes to neighbor
        target-line

        (let
          [target-note-scale-index (scale-index key-vector (:note target-line-note))]

          (if (and (not up?)
                   (= 1 (note-degree key-vector (:note target-line-note)))
                   (= :minor (key-mode key-vector)))
            (neighbor
              target-line
              target-index
              false
              :m2
              (* 1/2 (:dur target-line-note)))

            (neighbor
              target-line
              target-index
              up?
              (interval-keyword
                (:note target-line-note)
                (note-at-scale-index key-vector (+ direction target-note-scale-index)))
              (* 1/2 (:dur target-line-note)))

      )))))))


