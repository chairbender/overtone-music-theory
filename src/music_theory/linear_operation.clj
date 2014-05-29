;Functions related to linear operations in tonal theory,
;where functions do stuff with tonal theory "lines" (sequences of notes with specific durations)

(ns music-theory.linear-operation
  (:use music-theory.line)
  (:use music-theory.interval))

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
  with the note at that index (tonal theory consonance), returns a new line with
  an arpeggiation performed at that index using the given note, where arpeggiation
  note durations equals the original note's duration, and the duration of the first
  note equals first-duration. up? indicates whether the arpeggiation should go from low
  note to high note or vice versa. first-duration must be less than the duration of the note
  at the given index in the line. arpeggiate-note must form a tonal theory consonant interval
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
                        [(line-note lower-note first-duration)
                         (line-note higher-note (- (:dur target-line-note) first-duration))]
                        index (inc index))
          (replace-line rearticulated-line
                        [(line-note higher-note first-duration)
                         (line-note lower-note (- (:dur target-line-note) first-duration))]
                        index (inc index)
                        ))))))

