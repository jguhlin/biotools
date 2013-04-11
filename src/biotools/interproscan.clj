(ns biotools.interproscan
      (:require clojure.java.io
            clojure.string
            [clojure.core.reducers :as r]
            [iota :as iota]))

(defrecord InterproscanResults [protein-id md5-digest length analysis analysis-match-id analysis-name start stop score status date ipr-terms description go-terms])

(defn- ^:private -parse
  [line]
    (let [
          [protein-id md5-digest length analysis 
           analysis-match-id analysis-name
           start stop score status date 
           ipr-terms ipr-label 
           go-terms] 
          (mapv clojure.string/trim (clojure.string/split line #"\t"))]
      
      (->InterproscanResults
        protein-id
        md5-digest
        (Integer/parseInt length)
        analysis
        analysis-match-id
        (if (clojure.string/blank? analysis-name) nil analysis-name)
        (Integer/parseInt start)
        (Integer/parseInt stop)
        (if (= "-" score) nil (Double/parseDouble score))
        status
        date
        ipr-terms
        ipr-label
        (if go-terms 
          (clojure.string/split go-terms #"\|")
          []
          ))))

(defn parse-lazy
  "Lazy parser. Slower method, given a reader parses the lines."
  ([rdr]
    (for [line (line-seq rdr)]
      (-parse line))))

(defn parse
  "Not lazy, but handles big files I believe. Faster parser using reducers. Only need to pass the filename, not a reader"
  ([filename]
    (->> (iota/vec filename)
        (r/map -parse)
        (r/foldcat))))