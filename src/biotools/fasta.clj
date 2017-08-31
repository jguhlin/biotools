(ns biotools.fasta
  "This namespace contains functions for parsing and interpreting FASTA files."
  (:require [clojure.string :as string]
            [iota :as iota]))

(defn -parse-fasta-file
  "Internal function, please do not use directly.
  Read the data from the given reader as a list of strings, where
  each string is made up of multiple lines, separated by // on it's own
  line."
  [fasta]

  (->> fasta
       (partition-by #(= \> (first %)))
       (filter (comp not #(= \> (first %))))))

(defn parse
  "Parses FASTA file when given a reader, an example is:
  
  ```clojure
    (require 'biotools.fasta :as 'fasta)
  
    (with-open [rdr (clojure.java.io/reader \"test.fasta\")]
      (into {}
          (map 
            (fn [r] {(:id r) (count (:seq r))})
            (fasta/parse rdr))))))
  ```
  This method is useful for delayed functions and controlling when file reader closes."
  [rdr]
  (for [[header fasta_sequence] (partition 2 (-parse-fasta-file (line-seq rdr)))]
    {:header_raw 
     (apply str header) 
     :header 
     (subs 
       (apply str header) 
       1) 
     :id 
     (first 
       (clojure.string/split (subs (apply str header) 1) #"\s")) 
     :seq (vec 
            (string/replace 
              (apply str fasta_sequence) 
              #"\s" ""))}))
     

(defn parse-from-seq
  "Parse FASTA sequence directly from a sequence."
  [fasta]
  (for [[header fasta_sequence] (partition 2 (-parse-fasta-file fasta))]
    {:header_raw (apply str header) :header (subs (apply str header) 1) :id (first (clojure.string/split (subs (apply str header) 1) #"\s")) :seq (vec (string/replace (apply str fasta_sequence) #"\s" ""))}))

(defmacro process-fasta 
  "Easy to use FASTA parsing macro. Loops over body for each FASTA seq in a multifasta file.
  Handles file opening and closing. Returns a sequence.
  
  ```clojure
    (process-fasta \"test.fasta\"
      [(:id entry) (count (:seq entry))]
  ```"
  [filename & body]
  `(with-open [rdr# (clojure.java.io/reader ~filename)]
     (doall 
       (for [~'entry (parse rdr#)]
         (do ~@body)))))
  
  
  
  
  
  
  
  
