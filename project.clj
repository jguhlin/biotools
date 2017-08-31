(defproject biotools "0.1.1-b1"
  :main biotools.core
  :description "Bioinformatic tools for clojure. Used in internal projects. Hoping this helps someone. Feel free to contribute!"
  :url "http://github.com/jguhlin/biotools"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :plugins [[lein-codox "0.10.3"]]
  :dependencies [
                 [org.clojure/clojure "1.8.0"]
                 [iota "1.1.3"]
                 [clojure-csv/clojure-csv "2.0.2"]])
                 
