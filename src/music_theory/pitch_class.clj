;Stuff involving pitch classes.
;Pitch classes are represented by pitch class keywords of the form :<naturalnote><alterations>,
;where naturalnote is a-g or A-G and alterations is either any number of flats (b) or any number of sharps (#)


(ns music-theory.pitch-class
  (:use clojure.set))


(def ^{:private true :doc "Map from one natural pitch-class to the next natural pitch class"}
  next-natural-pitch-class-map (hash-map :A :B :B :C :C :D :D :E :E :F :F :G :G :A))
(def ^{:private true :doc "Map from one natural pitch-class to the previous natural pitch class"}
	previous-natural-pitch-class-map (map-invert next-natural-pitch-class-map))

(defn- normalize-pitch-class
  "Uppercases pitch-class keywords"
  [pitch-class]
  (let [pitch-class-name (name pitch-class)]
    (keyword (str (.toUpperCase (str (.charAt pitch-class-name 0)))
                  (when (> (count pitch-class-name) 1)
                    (.substring pitch-class-name 1 (count pitch-class-name)))))))

(defn natural
	"Returns the passed keyword but with any alteration removed"
	[pitch-class]
	(keyword (str (first (name (normalize-pitch-class pitch-class))))))



(defn- accidentalify
	"Adds the passed number of accidentals to the passed
	pitch class keyword and returns that as a new keyword.
	If nil is passed for num-accidentals, returns the pitch-class unmodified."
	[pitch-class num-accidentals accidental]
 (let [normal-pitch-class (normalize-pitch-class pitch-class)]
   (if (nil? num-accidentals)
		normal-pitch-class
		(keyword
		 (apply str
			(flatten
			 	[(name normal-pitch-class)
				(vec (for [i (range num-accidentals)]
					accidental))])))
		)))

(defn sharpen
	"Adds the passed number of sharps to the passed
	pitch class keyword and returns that as a new keyword.
	If nil is passed for num-sharps, returns the pitch-class unmodified."
	[pitch-class num-sharps]
	(accidentalify (normalize-pitch-class pitch-class) num-sharps "#"))

(defn flattify
	"Adds the passed number of flats to the passed
	pitch class keyword and returns that as a new keyword.
	If nil is passed for num-flats, returns the pitch-class unmodified."
	[pitch-class num-flats]
	(accidentalify (normalize-pitch-class pitch-class) num-flats "b"))

(defn flats
  "Counts the number of flats in the pitch-class and returns the count"
  [pitch-class]
  (count (filter #{\b} (name (normalize-pitch-class pitch-class)))))

(defn sharps
  "Counts the number of sharps in the pitch-class and returns the count"
  [pitch-class]
  (count (filter #{\#} (name (normalize-pitch-class pitch-class)))))

(defn pitch-class-equals
  "True if they represent the same pitch class.
  Enahrmonic equivalence doesn't count as being equal."
  [pitch-class other-pitch-class]
  (= (normalize-pitch-class pitch-class) (normalize-pitch-class other-pitch-class)))

(defn next-natural-pitch-class
	"Returns the 'natural' pitch class that is one higher
	than the given pitch class. 'Natural' here means without an accidental.
	So, if :Bb is given, the next natural will be :C. If B# is given, the next natural will
	be :C"
	[pitch-class]
	(next-natural-pitch-class-map (natural (normalize-pitch-class pitch-class))))

(defn previous-natural-pitch-class
	"Returns the 'natural' pitch class that is one lower
	than the given pitch class. 'Natural' here means without an accidental.
	So, if :Bb is given, the previous natural will be :A. If B# is given, the previous natural will
	be :A"
	[pitch-class]
	(previous-natural-pitch-class-map (natural (normalize-pitch-class pitch-class))))
