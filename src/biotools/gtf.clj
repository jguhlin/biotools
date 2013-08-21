(ns biotools.gtf
  (:require [clojure.core.reducers :as r]))

(defrecord GTF-Entry [landmark source type start end score strand frame attributes])

(defn ^:private -parse-attributes [attributes]
	(->> 
   (clojure.string/split attributes #"\s*;\s*")
   (map #(clojure.string/split % #"[ =]"))
   (map (fn [[k v]] [(keyword (clojure.string/lower-case k)) (clojure.string/replace v #"\"" "")]))
   (into {})))

(defn ^:private -parse [line]
	(if-not (or (empty? line) (= \# (first line)))
		(let [[landmark source gtf_type start end score strand phase attributes] (clojure.string/split line #"\t")]
         (conj (zipmap [:landmark :source :type :start :end :score :strand :phase]
                           [landmark source gtf_type (Integer/parseInt start) (Integer/parseInt end) score strand phase])
                    (-parse-attributes attributes)))
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
  [rdr]
  (r/fold combine (r/map (partial -parse) (line-seq rdr))))

; need to check that everything is working!!


