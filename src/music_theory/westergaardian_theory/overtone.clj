(ns
    ^{:doc "for interoperability with overtone - grants the ability to play lines"
       :author "Kyle Hipke"}
    music-theory.westergaardian-theory.overtone
  (:use music-theory.westergaardian-theory.note)
  (:use music-theory.westergaardian-theory.line)
	(:use music-theory.westergaardian-theory.semitone)
  (:require [overtone.core :as overtone]))


(defn- play-line-recurse
  "Private method to implement play-line"
  [target-line beat-number metro instrument note-index]
  (overtone/at (metro beat-number) (instrument (semitones->midi (note-semitones (:note (line-note-at target-line note-index))))))
  ;recur but only evaluate when it's time to play the note, so it responds to changing metronome
  (let [next-beat (+ (:dur (line-note-at target-line note-index)) beat-number)]
    (overtone/apply-by (metro next-beat)
            #'play-line-recurse target-line next-beat metro instrument (inc note-index) []))
  )

(defn play-line
  "Plays the line, starting playback at the specified
  beat number of the passed metronome (metro).If
  you change the metronome tempo during playback, the line playback will change accordingly.
  Beat-number is a number representing the number of beats from the start of the metronome
  that playback should begin at. So a beat-number of 50 means start playing the line
  on the 50th beat of the metronome (50.25 would mean play at a quarter from the start
  of the 50th beat). Instrument is an instrument defined by definst, which will be used
  to play the note (it should accept a midi note value, not a frequency, as its argument).
	metro is an overtone metronome function (so something like (metronome 100)),
  that will be used to determine when to play the notes.
  "
  [target-line beat-number metro instrument]
  (play-line-recurse target-line beat-number metro instrument 0))
