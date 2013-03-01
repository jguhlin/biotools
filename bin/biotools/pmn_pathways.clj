(ns biotools.pmn-pathways)

(set! *warn-on-reflection* true)

(defrecord PMNPathway [pathway-id
                       pathway-name
                       reaction-id
                       EC
                       protein-id
                       protein-name
                       gene-id
                       gene-name])

(defn ^:private -parse
  [line]
      (apply ->PMNPathway (mapv clojure.string/trim (clojure.string/split line #"\t"))))

(defn parse-reader 
  ([rdr]
    (let [line-seq-reader (line-seq rdr)
          lines (rest line-seq-reader)] ; Skip the header line
      (for [line lines]
        (-parse line)))))