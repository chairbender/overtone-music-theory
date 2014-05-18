;some utility functions
(ns music-theory.utility)

(defn occurrences
  "Counts the number of times substring occurs in string."
  [string substring]
  (count (re-seq (re-pattern substring) string)))
