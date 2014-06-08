(defproject music-theory "0.1.0-SNAPSHOT"
  :description "Music theory (specifically, based on Peter Westergaard's tonal theory) library for Overtone, to facilitate easily using musical structures and concepts to generate sounds."
  :url "https://github.com/kwhipke1/music-theory"
  :license {:name "The MIT License"
            :url "http://opensource.org/licenses/MIT"}
  :test-paths ["test"]
  :source-paths ["src"]
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [org.clojure/math.numeric-tower "0.0.4"]
                 [midje "1.5.1"]
                 [overtone "0.9.1"]])
