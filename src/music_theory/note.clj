;operations on notes (tonal-pitch-class with an octave)
;Note keywords are in the same format as overtone, but unlike with overtone the note letters matter so
;these functions don't work on midi note integer values.
(ns music-theory.interval)

(defn note-letter
	"Returns the capital letter of the note keyword.
	So :Bb3 returns the character 'B'"
	[note]
	(.charAt (name note) 0))

(defn note-octave
	"Returns the octave of the given note keyword as an integer."
	[note]
	(if (.contains (name note)  "-")
		(* -1 (read-string (str (.charAt (name note) (dec (count (name note)))))))
		(read-string (str (.charAt (name note) (dec (count (name note))))))))
