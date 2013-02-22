(ns biotools.gff
  (:require [clojure.core.reducers :as r]))

(defrecord GFF-Entry [landmark source type start end score strand phase attributes id parent note conf_class node])

(defn ^:private -parse-attributes [attributes]
	(->> 
   (clojure.string/split attributes #";")
   (map #(clojure.string/split % #"="))
   (map (fn [[k v]] [(keyword (clojure.string/lower-case k)) v]))
   (into {})
   ))

;defrecord did NOT speed anything up
(defn ^:private -parse [species version line]
	(if-not (or (empty? line) (= \# (first line)))
		(let [[landmark source gff_type start end score strand phase attributes] (clojure.string/split line #"\t")]
         (conj (zipmap [:landmark :source :type :start :end :score :strand :phase :species :version]
                           [landmark source gff_type (Integer/parseInt start) (Integer/parseInt end) score strand phase species version])
                    (-parse-attributes attributes)))
    nil))

;defrecord did NOT speed anything up
(defn ^:private -parse-temp-disabled [species version line]
	(if-not (or (empty? line) (= \# (first line)))
		(let [[landmark source gff_type start end score strand phase attributes] (map str (clojure.string/split line #"\t"))]
         (conj (zipmap [:landmark :source :type :start :end :score :strand :phase :species :version]
                           [(str landmark) source gff_type (read-string start) (read-string end) score strand phase species version])
                    (-parse-attributes attributes)))
    nil))


(defn ^:private -orig-parse [line]
	(if-not (= \# (first line))
		(let [[landmark source gff_type start end score strand phase attributes] (map str (clojure.string/split line #"\t"))]
         (into {} [(zipmap [:landmark :source :type :start :end :score :strand :phase]
                           [(str landmark) source gff_type (read-string start) (read-string end) score strand phase])
                    (first (-parse-attributes attributes))]))
    nil))


(defn parse-file 
  "Given the filename,process the GFF file and return the attributes in a map, with keys for easy access. 
   Not lazy."
  [filename]
  
  (with-open [rdr (clojure.java.io/reader filename)]
  (doseq [entry (line-seq rdr)]
    (-parse entry)
    )))

(defn parse-reader 
  "Parse GFF from a reader. Maybe lazy."
  [rdr]
  (keep identity 
        (for [entry (line-seq rdr)]
          (-parse entry))))

(defn combine
  ([] '())
  ([x y] (if (not (nil? y)) (conj x y) x)))

(defn parse-reader-reducer
  "Experimental..."
  [rdr species version]
  (r/fold combine (r/map (partial -parse species version) (line-seq rdr))))

; need to check that everything is working!!


