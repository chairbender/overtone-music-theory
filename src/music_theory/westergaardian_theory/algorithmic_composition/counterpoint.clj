(ns
    ^{:doc "functions for doing counterpoint using westergaardian theory"
       :author "Kyle Hipke"}
    music-theory.westergaardian-theory.algorithmic-composition.counterpoint
  (:use music-theory.westergaardian-theory.line)
  (:use music-theory.westergaardian-theory.interval)
  (:use music-theory.westergaardian-theory.key)
  (:use music-theory.westergaardian-theory.note)
  (:use music-theory.westergaardian-theory.tonal-pitch-class)
  (:use music-theory.westergaardian-theory.linear-operation)
  (:use music-theory.westergaardian-theory.tonal-utility)
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
  (let [scale (ascale key-vector octave)]

      (let [starting-note (cond
                        (= first-note-interval-number 3)
                        (get scale 2)

                        (= first-note-interval-number 5)
                        (get scale 4)

                        (= first-note-interval-number 8)
                        (interval-above (get scale 0) :P8))
            ending-note (get scale 0)]
        (full-diatonic-step-motion key-vector starting-note ending-note))))

(defn basic-arpeggiation
  "Generates a basic arpeggiation for the bass line, using the operation rules
  of Tonal Theory. Final and first pitch must be tonics. The middle pitch must be
  a fifth above or fourth below the final tonic. If the middle pitch is more than
  a fifth from the first pitch, a triad pitch must be inserted between the middle pitch and first pitch to
  make there be no intervals larger than an octave, but that
  insertion must follow the rules for bass line secondary structure triad inserts.
  The starting and ending pitches can't be larger than an octave, since that is undefined
  by westergaardian theory (triad ptiches are supposed to be inserted, but no operation could
  be performed because any triad pitch insertion would create a skip larger than an octave
  with one of the notes in the event the middle note is chosen to be
  a fourth below the final pitch). Whole notes will be used for all notes
  key should be a key vector indicating the key to use. first-octave should be
  an integer indicating the octave of the first tonic. last-octave should be an integer
  indicating the octave of the final tonic. middle-up? should be true if the middle note
  should be a fifth above the final pitch, otherwise it will be a fourth below it. insert-note should
  be a note that should be put between the first and middle pitch in the event they are more
  than a fifth apart. It must follow the rules for triad inserts for bass line secondary structures. It
  is ignored if the middle note is a fifth or less apart from the first note."
  [key first-octave last-octave middle-up? insert-note]
  (let [first-scale (ascale key first-octave)
        last-scale (ascale key last-octave)]

    (let [middle-note (if middle-up?
                        (get last-scale 4)
                        (get (ascale key (dec last-octave)) 4)
                        )]
      (if (> (interval-number (interval-keyword middle-note (get first-scale 0))) 5)

        (line
          (get first-scale 0) 1
          insert-note 1
          middle-note 1
          (get last-scale 0) 1)

        (line
          (get first-scale 0) 1

          middle-note 1

          (get last-scale 0) 1
          )))))


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
  [target-line key-vector index upper-line?]
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
                    (consonant? (compound-interval-keyword (get triad-pitches i) (:note line-note-before)) (not upper-line?))
                    (consonant? (compound-interval-keyword (get triad-pitches i) (:note line-note-after)) (not upper-line?))
                    (<= (interval-number (interval-keyword (get triad-pitches i) (:note line-note-after))) 8))
              (get triad-pitches i))))))))

(defn valid-triad-inserts
  "Returns a map of integers to vectors, where
  each integer represents an index in target-line (of a note), and
  the vector it maps to indicates the ntoes that could be inserted
  BEFORE that note,, following the rules of westergaardian theory counterpoint
  for the upper and bass lines.
  target-line is the line to examine and key-vector is the
  kev-vector for the key that the target line is in.
  The rules are:

  Any triad pitch may precede the first pitch or be inserted between
  two consecutive pitches as long as no dissonant skip and no skip larger than
  an octave is created (and we'll consider a perfect fourth skip in an upper line
  to be consonant). upper-line? indicates whether this is true. upper-line being true also means
  inserts are allowed before the first note, otherwise they aren't"
  [target-line key-vector upper-line?]
  (apply hash-map
    (filter
      #(not (nil? %))
      (for [i (range (if upper-line? 0 1) (line-note-count target-line))
          output-index? [true false]]
        (let [valid-inserts (valid-triad-inserts-before target-line key-vector i upper-line?)]
          (when (not-empty valid-inserts)
            (if output-index?
              i
              valid-inserts)))))))

(defn valid-triad-inserts-between
  "Like valid-triad-inserts, but only returns a single vector that has the valid
  inserts between the two given notes"
  [first-note second-note key-vector upper-line?]
  (valid-triad-inserts-before
    (line first-note 1 second-note 1)
    key-vector
    1
    upper-line?
    )
  )

(defn valid-step-motion-inserts
  "Returns a vector containing the indexes of notes where the
  following note forms a skip (interval a 3rd or larger) with the indexed note,
  and where the number of notes that would be inserted between the two notes
  in a step motion is <= limit."
  [target-line limit]
  (vec
    (filter
      #(not (nil? %))
      (for [i (range (dec (line-note-count target-line)))]
        (when (and
                (<= (interval-number (interval-keyword
                                       (:note (line-note-at target-line i))
                                       (:note (line-note-at target-line (inc i))))) (+ 2 limit))
                ( > (interval-number (interval-keyword
                                 (:note (line-note-at target-line i))
                                 (:note (line-note-at target-line (inc i))))) 2))
          i)))))

(defn valid-triad-repeats
  "Returns a vector containing the indexes of notes that
  are triad pitches of the key, meaning those notes are allowed to be
  repeated in westergaardian counterpoint."
  [line key-vector]
  (vec
    (filter
      #(not (nil? %))
      (for [i (range (line-note-count line))]
        (when (contains? [1 3 5]
                        (note-degree key-vector (:note (line-note-at line i))))
          i
          )))))

(defn counterpoint-step-motion
  "Returns a step motion line starting on the
  first note and ending on the second (all notes being a whole note),
  following the rules for upper and bass line step motion (they are the same):
  uses the diatonic degrees except for the special cases
  where it uses the raised sixth or seventh degree in a minor key:
  a) a rising step motion from the fifth degree to the tonic
  b) a rising step motion from the fifth degree to the seventh
  c) a falling step motion from the raised seventh to the fifth"
  [starting-note ending-note key-vector]
  (let [diatonic-motion (full-diatonic-step-motion key-vector starting-note ending-note)]
    (cond
      ;rising motion from fifth to tonic, and minor
      (and (= (higher-note starting-note ending-note) ending-note)
           (= 5 (note-degree key-vector starting-note))
           (= 1 (note-degree key-vector ending-note))
           (= :minor (key-mode key-vector)))
      ;raise the sixth and seventh
      (modify-note-at raise
        (modify-note-at raise diatonic-motion 1) 2)

      ;rising motion from fifth to seventh, and minor
      (and (= (higher-note starting-note ending-note) ending-note)
           (= 5 (note-degree key-vector starting-note))
           (= 7 (note-degree key-vector ending-note))
           (= :minor (key-mode key-vector)))
      ;raise the sixth
      (modify-note-at raise diatonic-step-motion 1)

      ;falling motion from teh raised seventh to the fifth, and minor
      (and (= (higher-note starting-note ending-note) starting-note)
           (= 7 (note-degree key-vector (lower starting-note)))
           (= 5 (note-degree key-vector ending-note))
           (= :minor (key-mode key-vector)))
      ;raise the sixth
      (modify-note-at raise diatonic-step-motion 1)

      :else
      diatonic-motion)))
