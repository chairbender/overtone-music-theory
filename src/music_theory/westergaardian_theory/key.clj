;functions involving the notes and tonal pitch classes of a given key/scale

(ns music-theory.westergaardian-theory.key
  (:use music-theory.westergaardian-theory.diatonic-collection)
  (:use music-theory.westergaardian-theory.note)
  (:use music-theory.westergaardian-theory.tonal-pitch-class)
  (:use music-theory.westergaardian-theory.interval)
  (:use music-theory.westergaardian-theory.tonal-utility))

(defn- rotate-key-vector
  "rotates the vector forward by the given offset, for example, if the vector is
  [I II III IV V VI VII] and the offset is 5, the new vector is
  [III IV V VI VII I II] (notice how the I moved up 5 places"
  [key-vector offset]
  [
    (get key-vector (mod (- offset) 7))
    (get key-vector (mod (- 1 offset) 7))
    (get key-vector (mod (- 2 offset) 7))
    (get key-vector (mod (- 3 offset) 7))
    (get key-vector (mod (- 4 offset) 7))
    (get key-vector (mod (- 5 offset) 7))
    (get key-vector (mod (- 6 offset) 7))])

(defn key-vector
  "Returns a vector of the tonal pitch classes that comprise the specified key.
  The vector contains, in order, the I, II, II, IV, V, VI, and VII diatonic degrees
  of the key. tonic indicates which tonal pitch class should be the tonic of the
  key, and mode can be :major or :minor and indicates the type of tonic triad
  (chord consisting of I III V degrees of the scale) that the key should have"
  [tonic mode]
  (if (= mode :major)
    (diatonic-collection tonic)
    ;construct the minor from the relative major
    (let [relative-major (diatonic-collection
                           (tonal-pitch-class-from-note
                             (interval-above (note-from-tonal-pitch-class tonic 4) :m3)))]
      (rotate-key-vector relative-major 2)
      )))

(defn key-tonic
  "returns the tonic tonal pitch class of the key vector"
  [key-vector]
  (get key-vector 0))

(defn key-mode
  "returns the mode of the key vector as :major or :minor"
  [key-vector]
  (if (= :m3
        (interval-keyword
          (note-from-tonal-pitch-class (get key-vector 0) 4)
          (note-from-tonal-pitch-class (get key-vector 2) 4)))
    :minor
    :major
    ))

(defn note-degree
  "given a note and a key vector, return the degree of the note in the key
  as an integer from 1-7 (inclusive). note must have a tonal pitch class of one of the key's diatonic degrees."
  [key-vector note]
  (inc (.indexOf key-vector (tonal-pitch-class-from-note note))))

(defn scale
  "Given a key vector and starting octave,
  returns a vector of 7 notes representing the single-octave scale of the
  key, starting at the given octave and ending at degree VII."
  [key starting-octave]
  (vec
    (if (= (tonal-pitch-class-letter (get key 0)) \A)
      (map #(note-from-tonal-pitch-class % starting-octave) key)
      ;need to add an octave change since we aren't starting on A
      (let [octave-switch-index (.indexOf (vec (map #(natural %) key)) :A)]
        (for [x (range 7)]
          (if (>= x octave-switch-index)
            (note-from-tonal-pitch-class (get key x) (inc starting-octave))
            (note-from-tonal-pitch-class (get key x) starting-octave))))))
  )


(defn scale-index
  "Given a key vector and a note, returns that note's
  'scale index' for the scale of that key. the note must
  be in the scale.
  The scale index is defined to be 0 at octave 0 of the tonic (IO), then
  1, 2, 3, and so on at II0 III0 IV0. -1, -2, and so on
  corresponds to VII-1, VI-1 and so on."
  [key-vector note]
  (note-letter-difference note (get (scale key-vector 0) 0) ))

(defn note-at-scale-index
  "Given a key-vector and scale index, returns the
  note that would be at that scale index."
  [key-vector index]

  (get
    (scale key-vector (if (>= index 0) (quot index 7) (dec (quot index 7))))
    (mod index 7))

  )