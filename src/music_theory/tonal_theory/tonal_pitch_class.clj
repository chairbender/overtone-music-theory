;Stuff involving tonal pitch classes.
;Tonal Pitch classes are represented by tonal pitch class keywords of the form :<naturalnote><alterations>,
;where naturalnote is a-g or A-G and alterations is either any number of flats (b) or any number of sharps (#)
;We use the word "tonal" to describe these pitch classes to indicate that enharmonic equivalence is not assumed,
;unlike with standard pitch classes.


(ns music-theory.tonal-theory.tonal-pitch-class
  (:use clojure.set))



(def ^{:private true :doc "Map from one natural tonal-pitch-class to the next natural pitch class"}
  next-natural-tonal-pitch-class-map (hash-map :A :B :B :C :C :D :D :E :E :F :F :G :G :A))
(def ^{:private true :doc "Map from one natural tonal-pitch-class to the previous natural pitch class"}
	previous-natural-tonal-pitch-class-map (map-invert next-natural-tonal-pitch-class-map))

(defn- normalize-tonal-pitch-class
  "Uppercases tonal-pitch-class keywords"
  [tonal-pitch-class]
  (let [tonal-pitch-class-name (name tonal-pitch-class)]
    (keyword (str (.toUpperCase (str (.charAt tonal-pitch-class-name 0)))
                  (when (> (count tonal-pitch-class-name) 1)
                    (.substring tonal-pitch-class-name 1 (count tonal-pitch-class-name)))))))

(defn natural
	"Returns the passed keyword but with any alteration removed"
	[tonal-pitch-class]
	(keyword (str (first (name (normalize-tonal-pitch-class tonal-pitch-class))))))


(defn- accidentalify
	"Adds the passed number of accidentals to the passed
	pitch class keyword and returns that as a new keyword.
	If nil is passed for num-accidentals, returns the tonal-pitch-class unmodified."
	[tonal-pitch-class num-accidentals accidental]
 (let [normal-tonal-pitch-class (normalize-tonal-pitch-class tonal-pitch-class)]
   (if (nil? num-accidentals)
		normal-tonal-pitch-class
		(keyword
		 (apply str
			(flatten
			 	[(name normal-tonal-pitch-class)
				(vec (for [i (range num-accidentals)]
					accidental))])))
		)))


(defn flats
  "Counts the number of flats in the tonal-pitch-class and returns the count"
  [tonal-pitch-class]
  (count (filter #{\b} (name (normalize-tonal-pitch-class tonal-pitch-class)))))

(defn sharps
  "Counts the number of sharps in the tonal-pitch-class and returns the count"
  [tonal-pitch-class]
  (count (filter #{\#} (name (normalize-tonal-pitch-class tonal-pitch-class)))))

(defn sharpen
	"Adds the passed number of sharps to the passed
	pitch class keyword and returns that as a new keyword.
	If nil is passed for num-sharps, returns the tonal-pitch-class unmodified."
	[tonal-pitch-class num-sharps]
	(accidentalify (normalize-tonal-pitch-class tonal-pitch-class) num-sharps "#"))

(defn unsharpen
  "removes the passed number of sharps from the tonal-pitch-class. requires that the pitch class actually
  has >= num-sharps."
  [tonal-pitch-class num-sharps]
  (let [prev-num-sharps (sharps tonal-pitch-class)]
    (sharpen (natural tonal-pitch-class) (- prev-num-sharps num-sharps))))

(defn flattify
	"Adds the passed number of flats to the passed
	pitch class keyword and returns that as a new keyword.
	If nil is passed for num-flats, returns the tonal-pitch-class unmodified."
	[tonal-pitch-class num-flats]
	(accidentalify (normalize-tonal-pitch-class tonal-pitch-class) num-flats "b"))

(defn unflattify
  "removes the passed number of flats from the tonal-pitch-class. requires that the pitch class actually
  has >= num-flats."
  [tonal-pitch-class num-flats]
  (let [prev-num-flats (flats tonal-pitch-class)]
    (flattify (natural tonal-pitch-class) (- prev-num-flats num-flats))))


(defn tonal-pitch-class-equals
  "True if they represent the same pitch class.
  Enahrmonic equivalence doesn't count as being equal."
  [tonal-pitch-class other-tonal-pitch-class]
  (= (normalize-tonal-pitch-class tonal-pitch-class) (normalize-tonal-pitch-class other-tonal-pitch-class)))

(defn next-natural-tonal-pitch-class
	"Returns the 'natural' pitch class that is one higher
	than the given pitch class. 'Natural' here means without an accidental.
	So, if :Bb is given, the next natural will be :C. If B# is given, the next natural will
	be :C"
	[tonal-pitch-class]
	(next-natural-tonal-pitch-class-map (natural (normalize-tonal-pitch-class tonal-pitch-class))))

(defn previous-natural-tonal-pitch-class
	"Returns the 'natural' pitch class that is one lower
	than the given pitch class. 'Natural' here means without an accidental.
	So, if :Bb is given, the previous natural will be :A. If B# is given, the previous natural will
	be :A"
	[tonal-pitch-class]
	(previous-natural-tonal-pitch-class-map (natural (normalize-tonal-pitch-class tonal-pitch-class))))

;map indicating, for each natural TPC, how many semitones it is away from A
(def ^{:private true} letter-semitone-count
  {:A 0 :B 2 :C 3 :D 5 :E 7 :F 8 :G 10})

(defn tonal-pitch-class-semitone-distance
  "Returns an integer indicating the offset in semitones of other-tpc from tpc, if
  they were notes in the same octave."
  [tonal-pitch-class other-tonal-pitch-class]
  (+ (- (sharps other-tonal-pitch-class) (sharps tonal-pitch-class))
     (- (flats tonal-pitch-class) (flats other-tonal-pitch-class))
     (- (get letter-semitone-count (natural other-tonal-pitch-class))
        (get letter-semitone-count (natural tonal-pitch-class)))
     )
  )


(defn tonal-pitch-class-letter
  "Returns the letter of the tonal pitch class as a char"
  [tonal-pitch-class]
  (let [normal-pitch-class (normalize-tonal-pitch-class tonal-pitch-class)] (.charAt (name normal-pitch-class) 0)))

(defn tonal-pitch-class-alterations
  "Returns the alterations of the tonal pitch class
  as a string. Empty string if none."
  [tonal-pitch-class]
  (or (re-find #"[#b]+" (name (normalize-tonal-pitch-class tonal-pitch-class))) ""))


