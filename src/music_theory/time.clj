;Functionality for handling time and duration in the
;context of music
;Duration symbols are represented as

(ns music-theory.time)



(defn duration
  "Returns the time in milliseconds that the
  given durational value would last in the given time signature at
  the given tempo (in beats per minute).
  Duration should be a fraction or integer where 1 = whole note.
  (so 2 = double whole note, 1/2 = half note, etc...). In between values are allowed,
  for example a quarter note triplet would be (1/4 * 2/3) = (1/6). You can even do things like 1/7.
  Tempo should be a double representing the beats per minute.
  Beat duration should be a fraction representing the duration that a beat lasts (i.e. the bottom
  of the time signature). For example, (1/4) would mean each beat is a quarter note."
  ([duration] (duration 100 1/4))
  ([duration tempo] (duration tempo 1/4))
  ([duration tempo beat-duration]
  (* (/ 1 tempo) 60000 (/ 1 beat-duration) duration)))

