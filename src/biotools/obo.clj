(ns biotools.obo
  (:require [clojure.string :as string]
            [clojure.core.reducers :as r]
            [iota :as iota]))

(defn -parse-obo-file
  "Read the data from the given reader as a list of strings, where
each string is made up of multiple lines, separated by // on it's own
line."
  [reader]

  (partition 2
             (rest 
               (partition-by #(re-find #"^\[\w+\]" (apply str %)) (line-seq reader)))))

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
    (if (nil? v)
      (do
        (println "Keyword:" k "is nil")
        {:nilfound [true]})
      {(keyword k) (clojure.string/trim v)})
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
  (let [converted-data
        (apply merge-with into
               (map -convert-to-proper-map
                    (map #(re-matches #"(.+?):\s*(.+?)\s*(!.*)?" %)
                         (filter (complement clojure.string/blank?) data))))]
    (if (:nilfound converted-data)
      (do (println data) (println converted-data)))
    converted-data))

(defn parse
  [rdr]
  (for [term-data (-parse-obo-file rdr)
        :when (= (ffirst term-data) "[Term]")]
    (try (-convert (second term-data))
      (catch Exception e 
        (do
          (println)
          (println e)
          (println (.getCause e))
          (println (.printStackTrace e))
          (Thread/sleep 2000)
          (println)
          (println "Error running -convert")
          (println)
          (doall (map println term-data))
          (println)
          (System/exit 0))
      ))))

(defn parse-dbxref
  [dbxref-str]
  (let [[_ name _ description _ modifier] (re-find #"(\S+)\s?(\"(.*)\")?\s?(\{(.+)\})?" dbxref-str)]
     {:id name :description description :modifier modifier}))

(defn parse-dbxrefs
  [dbxref-str]
  (if (clojure.string/blank? dbxref-str) []
    (for [entry (clojure.string/split dbxref-str #"(?<!\\\\),")] ; Split by comma unless comma is escaped
      (parse-dbxref entry))))

(defn parse-def
  "Used to parse the definition lines(after broken into tag/value pairs):
   Should be passed a string that looked like \"This is an example\" [EC:2.1.2.1, GOH:pac]"
  [def-str]
  (if (clojure.string/blank? def-str)
    {}
    (let [[_ def _ dbxref] (re-find #"\"(.+)\"(\s+\[(.+)\])?" def-str)]
      {:def def :dbxref (parse-dbxrefs dbxref)}
      )))

(defn split-xref
  [xref-str]
  (let [[type id] (clojure.string/split xref-str #":")]
    (if (nil? id)
      (do (println "Invalid ID from parsing: " xref-str) (System/exit 0))
      {:type type :id id})))