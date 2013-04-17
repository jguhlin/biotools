(ns biotools.fasta
  (:require [clojure.string :as string]))

(defn -parse-fasta-file
  "Read the data from the given reader as a list of strings, where
each string is made up of multiple lines, separated by // on it's own
line."
  [reader]

  (->> (line-seq reader)
       (partition-by #(= \> (first %)))
       (filter (comp not #(= \> (first %))))))

(defn parse
  [rdr]
  (for [[header fasta_sequence] (partition 2 (-parse-fasta-file rdr))]
    {:header_raw (apply str header) :header (subs (apply str header) 1) :id (first (clojure.string/split (subs (apply str header) 1) #"\s")) :seq (vec (string/replace (apply str fasta_sequence) #"\s" ""))}))

