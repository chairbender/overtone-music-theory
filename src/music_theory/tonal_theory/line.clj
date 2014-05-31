;Functions for dealing with the concept of a "line" in tonal theory.
;a line is a sequence of notes, each with their own duration. There
;are no chords in a line, chords are formed as the result of simultaneously
;sounding notes in simultaneously sounding lines.

;lines are simply represented as vectors containing 'line-notes', where a line note
;is a map containing a :note and :dur entry (which have a note keyword and duration fraction, respectively)

(ns music-theory.tonal-theory.line)

(defn line-note
  "Returns a line note given a note and duration.

  More formally, returns a map of {:note note :dur duration}."
  [note duration]
  {:note note :dur duration}
  )

(defn line
  "Convenience method for creating a line without having to type out all of the
  curly braces and map key names.
  Number of parameters must be even, and the parameters should be of the form:
  note-keyword1 duration-fraction1, note-keyword2 duration-fraction2...
  where note-keyword is a note keyword for the first note and duration-fraction is
  a fraction representing the duration of the first note (1 being a whole note, 1/4 being a quarter note, etc...).
  Returns a line consisting of those notes with those durations.

  More formally, given parameters note1, duration1, note2, duration2 ...,
  returns a vector containing maps in the following form:
  [{:note note1 :duration duration1} {:note note2 :duration duration2}]"
  [& note-params]
  (let
      [params (vec note-params)]
      (vec
        (for [i (range 0 (count params) 2)]
          {:note (get params i) :dur (get params (inc i))}
          ))))

(defn replace-line
  "Replace the line-note(s) (note and duration) at the passed index (or index range)
  with the given line."
  ([original-line replacement-line start-index] (replace-line original-line replacement-line start-index start-index))
  ([original-line replacement-line start-index end-index]
  (let [end end-index
        start start-index]
    (vec
      (flatten
        (conj
          (subvec original-line 0 start)
          replacement-line
          (if (> (count original-line) 1)
            (subvec original-line (inc end) (count original-line))
            []
            )))))))

(defn line-note-at
  "Returns the line-note at the given index in the given line.
  A line-note is simply a map with a :note and :duration."
  [line index]
  (get line index))

(defn prepend-line-note
  "Prepends the line note to the line"
  [line-note line]
  (into [line-note] line)
  )

(defn append-line-note
  "appends the line note to the line"
  [line-note line]
  (conj line line-note)
  )

(defn line-duration
  "Returns the total duration of the line as a fraction"
  [line]
  (reduce #(+ %1 (:dur %2)) (:dur (get line 0)) (subvec line 1 (count line))))

(defn subline
  "like subvec, but operates on lines.
  Returns a new line consisting of the notes between
  start (inclusive) and end (exclusive)"
  [line start end]
  (subvec line start end))

(defn line-note-count
  "returns the number of notes in the line"
  [line]
  (count line))

(defn insert-line
  "inserts a line starting at the given index, moving the
  note at that current index forward to accomadate the inseted line."
  [target-line inserted-line index]
  (let [moved-line-note (line-note-at target-line index)]
    (replace-line
      target-line
      (append-line-note moved-line-note inserted-line)
      index)))

