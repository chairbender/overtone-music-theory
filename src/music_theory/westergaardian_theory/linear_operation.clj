;Functions related to linear operations in tonal theory,
;where functions do stuff with tonal theory "lines" (sequences of notes with specific durations)

(ns music-theory.westergaardian-theory.linear-operation
  (:use music-theory.westergaardian-theory.line)
  (:use music-theory.westergaardian-theory.interval)
  (:use music-theory.westergaardian-theory.key)
  (:use music-theory.westergaardian-theory.note))

(defn rearticulation
  "given a line and an index,
  return a line representing a rearticulation of the note at that index in the line
  (the note is split into two notes whose total duration equals the original note's duration,
  and who are the same note as the original). The duration of the first note in the
  rearticulation is equal to first-duration. first-duration must be a duration less than
  the duration of the rearticulated note in the given line."
  [target-line index first-duration]
  (let [line-note (line-note-at target-line index)]
    (replace-line
      target-line
      (line (:note line-note) first-duration
          (:note line-note) (- (:dur line-note) first-duration))
      index)))





(defn neighbor
  "Given a line an an index pointing at the first note of a rearticulation in that line, returns the line
  with a neighbor inserted between the rearticulation notes. A neighbor is simply a note a minor or major second
  above or below the two notes, caused by rearticulating the first note
  of the rearticulation, then moving it up or down a second. if up? is true, the neighbor will be above, otherwise it will be
  below. The duration of the neighbor will be neighbor-duration, and it must be less than
  the duration of the first note of the rearticulation. interval must be :m2 or :M2."
  [target-line index up? interval neighbor-duration]
  (let [target-line-note (line-note-at target-line index)]
    (let [rearticulated-line
          ;rearticulate the first note of the rearticulation
          (rearticulation target-line index (- (:dur target-line-note) neighbor-duration))]
      (replace-line rearticulated-line
        ;increment or decrement the second note of the new rearticulation
        [(line-note
          (if (= true up?)
            (interval-above (:note target-line-note) interval)
            (interval-below (:note target-line-note) interval))
          (:dur (line-note-at rearticulated-line (inc index))))]
        (inc index)
        ))))

(defn arpeggiate
  "Given a line, an index into that line, and a note forming a consonant interval
  with the note at that index (westergaardian theory consonance), returns a new line with
  an arpeggiation performed at that index using the given note, where arpeggiation
  note durations equals the original note's duration, and the duration of the first
  note equals first-duration. up? indicates whether the arpeggiation should go from low
  note to high note or vice versa. first-duration must be less than the duration of the note
  at the given index in the line. arpeggiate-note must form a westergaardian theory consonant interval
  (see the consonant? function in interval.clj)."
  [target-line index up? arpeggiate-note first-duration]
  (let [target-line-note (line-note-at target-line index)]
    (let [rearticulated-line
          ;rearticulate the note that will be the first of the arpeggiation
          (rearticulation target-line index first-duration)]
      (let [lower-note (lower-note arpeggiate-note (:note target-line-note))
            higher-note (higher-note arpeggiate-note (:note target-line-note))]
        (if up?
          (replace-line rearticulated-line
                        (line lower-note first-duration
                              higher-note (- (:dur target-line-note) first-duration))
                        index (inc index))
          (replace-line rearticulated-line
                        (line higher-note first-duration
                              lower-note (- (:dur target-line-note) first-duration))
                        index (inc index)
                        ))))))

(defn step-motion
  "Given a line and an index into the line that points at the first note
  of an arpeggiation, and a step-motion line,
  performs the step motion between the notes of the arpeggiation,
  using the notes in the step-motion line as if they are borrowed from the duration
  of the indexed note (so the indexed note will still be in the new line but it will be shorter
  by the duration of the step-motion line). The step-motion line must
  consist of notes going all up or all down, whose total duration must be less than the duration
  of the indexed note, with each note being some sort of 2nd interval from
  the preceeding note, and the notes must 'step' between the note in the line
  at the index and the one following it (the first note of the line
  must be a 2nd from the first note of the arpeggiation, and the last note
  of the line must be a 2nd from the last note of the arpeggiation. In other words, a step-motion causes
  a bunch of notes to be inserted between two notes forming an arpeggiation."
  [target-line index step-motion]
  (let [indexed-line-note (line-note-at target-line index)]
    (replace-line
      target-line
      (prepend-line-note (line-note (:note indexed-line-note) (- (:dur indexed-line-note) (line-duration step-motion))) step-motion)
      index
      )))

(defn delay-note
  "Given a line and index into that line, returns a new line where the
  note at that indexed is delayed, adding some of the duration of the first part of
  the indexed note to the duration of the note before it. duration indicates the duration to transfer,
  so a value of 1/2 would shorten the duration of the indexed note by 1/2 and add 1/2 to the duration of the
  note before it. duration must be less than the duration of the indexed note."
  [target-line index duration]
  (let
      [lengthened-note (line-note-at target-line index)
       shortened-note (line-note-at target-line (inc index))]
      (replace-line
    target-line
    (line (:note lengthened-note) (+ (:dur lengthened-note) duration)
          (:note shortened-note) (- (:dur shortened-note) duration))
    index
    (inc index))))

