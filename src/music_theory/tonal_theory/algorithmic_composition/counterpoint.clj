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


(defn valid-neighbor-indexes
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


(defn- triad-pitches-near
  "returns a vector containing the triad pitches of the key that are within
  an octave of the note"
  [note key-vector]
  (vec
    (filter
      #(not (nil? %))
      (for [cur-scale-index (range (- (scale-index key-vector note) 7) (+ (scale-index key-vector note) 8))]
        (when (contains? #{1 3 5} (note-degree key-vector (note-at-scale-index key-vector cur-scale-index)) )
          (note-at-scale-index key-vector cur-scale-index))))))

(defn- valid-triad-inserts-before
  "Returns avector containing the notes
  that would be valid to insert before the note at
  the given index"
  [target-line key-vector index]
  (let [line-note-before (if (= index 0) (line-note-at target-line index)(line-note-at target-line (dec index)))
        line-note-after (line-note-at target-line index)]
    ;generate the nearest triad pitches
    (let [triad-pitches (triad-pitches-near (:note line-note-before) key-vector)]
      ;check for consonance with both notes
      (vec
        (filter
          #(not (nil? %))
          (for [i (range (count triad-pitches))]
            ;true for is lowest sounding for the purposes of counterpoint
            (when (and
                    (consonant? (compound-interval-keyword (get triad-pitches i) (:note line-note-before)) true)
                    (consonant? (compound-interval-keyword (get triad-pitches i) (:note line-note-after)) true)
                    (<= (interval-number (interval-keyword (get triad-pitches i) (:note line-note-after))) 8))
              (get triad-pitches i))))))))

(defn valid-triad-inserts
  "Returns a map of integers to vectors, where
  each integer represents an index in target-line (of a note), and
  the vector it maps to indicates the ntoes that could be inserted
  BEFORE that note,, following the rules of tonal theory counterpoint.
  target-line is the line to examine and key-vector is the
  kev-vector for the key that the target line is in.
  The rules are:

  Any triad pitch may precede the first pitch or be inserted between
  two consecutive pitches as long as no dissonant skip and no skip larger than
  an octave is created (and we'll consider a perfect fourth skip in an upper line
  to be consonant)."
  [target-line key-vector]
  (apply hash-map
    (filter
      #(not (nil? %))
      (for [i (range (line-note-count target-line))
          output-index? [true false]]
        (let [valid-inserts (valid-triad-inserts-before target-line key-vector i)]
          (when (not-empty valid-inserts)
            (if output-index?
              i
              valid-inserts)))))))
