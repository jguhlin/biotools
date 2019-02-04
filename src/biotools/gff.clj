(ns biotools.gff
  (:require [clojure.core.reducers :as r]
            [biotools.fasta :as fasta]))

(defrecord GFF-Entry [landmark source type start end score strand phase attributes id parent note conf_class node])

(defn- ^:private -remove-quotes [val]
  (cond
    (nil? val)
    "" ; If a value is nil, has happened.
    (re-find #"\"(.+)\"" val)
    (second (re-matches #"\"(.+)\"" val))
    :else
    val))

(defn ^:private -parse-attributes [attributes]
  (->>
    (clojure.string/split attributes #";")
    (map #(clojure.string/split % #"="))
    (map (fn [[k v]] [(keyword (clojure.string/lower-case k)) (-remove-quotes v)]))
    (into {})))


(defn ^:private -parse [line]
  (if-not (or (empty? line) (= \# (first line)))
     (let [[landmark source gff_type start end score strand phase attributes] (clojure.string/split line #"\t")]
          (conj (zipmap [:landmark :source :type :start :end :score :strand :phase]
                        [landmark source gff_type (Integer/parseInt start) (Integer/parseInt end) score strand phase])
                (-parse-attributes attributes)))
     nil))

; Public function for -parse
; Not often used, but sometimes...
(defn parse-line [line]
 (-parse line))

(defn split-fasta
  "Splits the GFF3 file based on the FASTA directive."
  [rdr]
  (let [ls-rdr (line-seq rdr)]
    (split-with (fn [x] (not (re-find #"^\#\#FASTA" x))) ls-rdr)))

(defn parse-fasta
  [rdr]
  (fasta/parse-from-seq (drop 1 (second (split-fasta rdr)))))

(defn parse-file
  "Given the filename,process the GFF file and return the attributes in a map, with keys for easy access.
   Not lazy."
  [filename]

  (with-open [rdr (clojure.java.io/reader filename)]
   (doseq [entry (first (split-fasta rdr))]
     (-parse entry))))


(defn combine
  "Parse-reader-reducer helper fn"
  ([] '())
  ([x y] (if (not (nil? y)) (conj x y) x)))

(defn parse-reader
  "Parse GFF from a reader. Probably lazy."
  [rdr]
  (keep identity
        (for [entry (first (split-fasta rdr))]
          (-parse entry))))

(defn parse-reader-reducer
  "Parses the GFF file as a reducer."
  [rdr]
  (r/fold combine (r/map (fn [x] (-parse x)) (first (split-fasta rdr)))))
