;Functions related to linear operations in tonal theory,
;where functions do stuff with tonal theory "lines" (sequences of notes with specific durations)

(ns music-theory.linear-operation
  (:use music-theory.line))

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


;
;(defn neighbor
;  "Given a line an an index pointing at the first note of a rearticulation in that line, returns the line
;  with a neighbor inserted between the rearticulation notes. A neighbor is simply a note a minor or major second
;  above or below the two notes, caused by rearticulating the first note
;  of the rearticulation, then moving it up or down a second. if up? is true, the neighbor will be above, otherwise it will be
;  below. The duration of the neighbor will be neighbor-duration, and it must be less than
;  the duration of the first note of the rearticulation. interval must be :m2 or :M2."
;  [rearticulation-line up? interval neighbor-duration]
;  (let [line-note (line-note-at rearticulation-line 0)]
;    (line (:note line-note) (:dur line-note))
;
;
;    )
;  )

