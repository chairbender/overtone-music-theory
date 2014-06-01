
;functions for generating counterpoint lines by randomly performing
;any of the allowed options


(ns music-theory.tonal-theory.algorithmic-composition.random-counterpoint
  (:use music-theory.tonal-theory.line)
  (:use music-theory.tonal-theory.interval)
  (:use music-theory.tonal-theory.key)
  (:use music-theory.tonal-theory.note)
  (:use music-theory.tonal-theory.tonal-pitch-class)
  (:use music-theory.tonal-theory.linear-operation)
  (:use music-theory.tonal-theory.tonal-utility)
  (:use music-theory.utility)
  (:use music-theory.tonal-theory.algorithmic-composition.counterpoint))

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
                (* 1/2 (:dur target-line-note))))))))))


(defn random-triad-insert
  "Randomly inserts a triad pitch of the given key (I III or V)
  between two pitches or before the first pitch, but never creating
  a dissonant skip or a skip larger than an octave. The duration
   of the inserted pitch is always a whole note"
  [key-vector target-line]
  (let [valid-inserts-map (valid-triad-inserts target-line key-vector)]
    (let [chosen-entry (rand-nth (vec valid-inserts-map))]
      (let [chosen-index (get chosen-entry 0)
            chosen-note (rand-nth (get chosen-entry 1))]
        (insert-line
          target-line (line chosen-note 1) chosen-index)))))

(defn random-step-motion-insert
  "Inserts a step motion between two random consecutive notes that form a skip.
  Uses the diatonic degrees for the step motion except for the following
  rules, where it uses the raised sixth and seventh degrees in a minor key:
  a) a rising step motion from the fifth degree to the tonic
  b) a rising step motion from the fifth degree to the seventh
  c) a falling step motion from the raised seventh to the fifth

  does nothing if the target line has no valid places to do the insert
  "
  [target-line key-vector]
  (let [valid-indexes (valid-step-motion-inserts target-line)]
    (if (> (count valid-indexes) 0)
      (let [chosen-index (rand-nth valid-indexes)]
        (replace-line
          target-line
          (counterpoint-upper-step-motion
            (:note (line-note-at target-line chosen-index))
            (:note (line-note-at target-line (inc chosen-index)))
            key-vector)
          chosen-index
          (inc chosen-index)))
      target-line)))

