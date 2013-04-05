(ns biotools.obo
  (:require [clojure.string :as string]))

(defn -parse-obo-file
  "Read the data from the given reader as a list of strings, where
each string is made up of multiple lines, separated by // on it's own
line."
  [reader]

  (->> (line-seq reader)
       (partition-by #(= "[Term]" %))
       (filter (comp not #(= "[Term]" %)))))

(defn -convert-to-proper-map
  [[_ k v _]]
  (if (or
        (= k "alt_id")
        (= k "subset")
        (= k "xref")
        (= k "is_a")
        (= k "consider")
        (= k "synonym")
        (= k "intersection_of")
        (= k "relationship")
        (= k "replaced_by")
        (= k "disjoint_from")) ; Treat these like a vector, so they merge later appropriately...
    {(keyword k) [(clojure.string/trim v)]} ; Not pretty, but not sure a better way...
    {(keyword k) (clojure.string/trim v)}
    )
  
  )

(defn -convert
  "Converts to a map. Certain keys are turned into a vector, as appropriate. Here is an example:
{:is_a [\"GO:0048308\" \"GO:0048311\"], 
:synonym [\"\\\"mitochondrial inheritance\\\" EXACT []\"], 
:def \"\\\"The distribution of mitochondria, including the mitochondrial genome, into daughter cells after mitosis or meiosis, mediated by interactions between mitochondria and the cytoskeleton.\\\" [GOC:mcc, PMID:10873824, PMID:11389764]\", 
:namespace \"biological_process\", 
:name \"mitochondrion inheritance\", 
:id \"GO:0000001\"}
"
  [data]
  ;(apply merge-with 
;         (comp vec flatten vector)
  (apply merge-with into
         (map -convert-to-proper-map
              (map #(re-matches #"(.+): (.+?)\s*(!.*)?" %)
                   (filter (complement clojure.string/blank?) data)))))

(defn parse
  [rdr]
  (for [term-data (rest (-parse-obo-file rdr))
        :when (not (= (first term-data) "[Term]"))]
    (-convert term-data)))

(defn parse-dbxref
  [dbxref-str]
  (for [entry (clojure.string/split dbxref-str #",")]
    (let [[_ name _ description _ modifier] (re-find #"(\S+)\s?(\"(.*)\")?\s?(\{(.+)\})?" entry)]
      {:name name :description description :modifier modifier})))
