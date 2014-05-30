;functions involving the notes and tonal pitch classes of a given key/scale

(ns music-theory.key
  (:use music-theory.diatonic-collection)
  (:use music-theory.note)
  (:use music-theory.tonal-pitch-class)
  (:use music-theory.interval))

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

(defn key-tonal-pitch-classes
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

(defn scale
  "Given a note and a mode keyword (:major or :minor),
  returns a vector of 7 notes representing the single-octave scale of the
  key whose tonic has the same tonal pitch class as the starting
  note. for example :A4 :major would return the 7 notes of the A major scale,
  starting at :A4."
  [starting-note mode]
  (let [tonal-pitch-classes (key-tonal-pitch-classes (tonal-pitch-class-from-note starting-note) mode)
        starting-octave (note-octave starting-note)]
    (vec
      (if (= (tonal-pitch-class-letter (get tonal-pitch-classes 0)) \A)
        (map #(note-from-tonal-pitch-class % starting-octave) tonal-pitch-classes)
        ;need to add an octave change since we aren't starting on A
        (let [octave-switch-index (.indexOf (vec (map #(natural %) tonal-pitch-classes)) :A)]
          (for [x (range 7)]
            (if (>= x octave-switch-index)
              (note-from-tonal-pitch-class (get tonal-pitch-classes x) (inc starting-octave))
              (note-from-tonal-pitch-class (get tonal-pitch-classes x) starting-octave))))))))
