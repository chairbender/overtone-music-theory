



(ns
    ^{:doc "functions for generating counterpoint lines by randomly performing
any of the allowed options"
       :author "Kyle Hipke"}
    music-theory.westergaardian-theory.algorithmic-composition.random-counterpoint
  (:use music-theory.westergaardian-theory.line)
  (:use music-theory.westergaardian-theory.interval)
  (:use music-theory.westergaardian-theory.key)
  (:use music-theory.westergaardian-theory.note)
  (:use music-theory.westergaardian-theory.tonal-pitch-class)
  (:use music-theory.westergaardian-theory.linear-operation)
  (:use music-theory.westergaardian-theory.tonal-utility)
  (:use music-theory.utility)
  (:use music-theory.westergaardian-theory.algorithmic-composition.counterpoint))

(defn random-neighbor
  "Given a line, performs a random neighbor operation on
   consecutive notes that have a repeated pitch within the counterpoint
  rules (the neighbor is always a member of the diatonic collection except
  when it is the lower neighbor to the tonic in a minor key, in which case it is altered
  so as to be a minor second from the tonic. The duration of the neighbor will be a whole note
  Key-vector is the key vector of the key the line is in. target-line is the
  line to perform the operation on. Returns key-vector unmodified
   if there is no valid pitch repetition to neighbor."
  [key-vector target-line]
  (set-all-durations
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
                (* 1/2 (:dur target-line-note)))))))))

    1))


(defn random-upper-triad-insert
  "Following rules for upper line triad inserts,
  randomly inserts a triad pitch of the given key (I III or V)
  between two pitches or before the first pitch, but never creating
  a dissonant skip or a skip larger than an octave. The duration
   of the inserted pitch is always a whole note"
  [key-vector target-line]
  (let [valid-inserts-map (valid-triad-inserts target-line key-vector true)]
    (let [chosen-entry (rand-nth (vec valid-inserts-map))]
      (let [chosen-index (get chosen-entry 0)
            chosen-note (rand-nth (get chosen-entry 1))]
        (insert-line
          target-line (line chosen-note 1) chosen-index)))))

(defn random-bass-triad-insert
  "Like random-upper-triad-insert, but
  doesn't allow inserts before the first note"
  [key-vector target-line]
  (let [valid-inserts-map (valid-triad-inserts target-line key-vector true)]
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

  does nothing if the target line has no valid places to do the insert. The duration
  of the notes inserted are all whole notes.
  limit means only step motions that would insert <= limit notes will be performed.
  "
  [key-vector target-line limit]
  (let [valid-indexes (valid-step-motion-inserts target-line limit)]
    (if (> (count valid-indexes) 0)
      (let [chosen-index (rand-nth valid-indexes)]
        (replace-line
          target-line
          (counterpoint-step-motion
            (:note (line-note-at target-line chosen-index))
            (:note (line-note-at target-line (inc chosen-index)))
            key-vector)
          chosen-index
          (inc chosen-index)))
      target-line)))


(defn random-basic-arpeggiation
  "Creates a random basic arpeggiation line, following the rules for bass lines in
  westergaardian theory, in the given key. first-octave is an integer and is
   used as the octave of the first tonic."
  [key-vector first-octave]
  (let [chosen-last-octave (+ first-octave (rand-nth [-1 0 1]))
        chosen-middle-up? (rand-bool)]
    (let [first-scale (ascale key-vector first-octave)
          last-scale (ascale key-vector chosen-last-octave)]
      (let [first-note (get first-scale 0)
             middle-note (if chosen-middle-up?
                          (get last-scale 4)
                          (get (ascale key-vector (dec chosen-last-octave)) 4)
                          )]
        ;check if the middle note will be more than a fifth from
        ;the first
        (if (> (interval-number (interval-keyword middle-note (get first-scale 0))) 5)
          ;randomly pick an insert-note
          (let [valid-inserts (valid-triad-inserts-between first-note middle-note key-vector false)]
            (basic-arpeggiation key-vector first-octave chosen-last-octave chosen-middle-up? (rand-nth valid-inserts)))
          ;insert-note doesn't matter
          (basic-arpeggiation key-vector first-octave chosen-last-octave chosen-middle-up? :C4))))))

(defn random-triad-repeat
  "Randomly repeats a triad note in the line, always using a whole note
  for the new note."
  [key-vector target-line]
  (let [chosen-index (rand-nth (valid-triad-repeats target-line key-vector))]
    (let [chosen-note (:note (line-note-at target-line chosen-index))]
      (insert-line target-line
                   (line chosen-note 1)
                   chosen-index))))

(defn random-upper-line
  "Generates a random upper line in the given key at the given
  octave, randomly performing any allowed operation some number of times.
  key-vector indicates the key, octave indicates the octave of the starting note,
  and length determines the length of the resulting line in whole notes."
  [key-vector octave length]
  (let [starting-line (basic-step-motion key-vector octave (rand-nth [3 5 8]))]
    (loop [result-line starting-line]
      (if (>= (line-note-count result-line) length)
        result-line
        (let [chosen-function (rand-nth [random-triad-repeat,
                                         random-neighbor,
                                         random-upper-triad-insert,
                                         random-step-motion-insert] )]
          (if (= chosen-function random-step-motion-insert)
            (recur (random-step-motion-insert key-vector result-line (- length (line-note-count starting-line))))
            (recur (chosen-function key-vector result-line))
            ))))))

