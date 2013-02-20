(ns biotools.blast)

(defn ^:private -parse 
  [opts line]
  (if (:note opts)
    (zipmap [:query-id :subject-id :pct-identity :alignment-length :mismatches :gap-opens :query-start :query-end :subject-start :subject-end :evalue :bitscore :qlen :slen :species :version :note]
            (conj (mapv str (clojure.string/split line #"\t")) (:species opts) (:version opts) (:note opts)))
    (zipmap [:query-id :subject-id :pct-identity :alignment-length :mismatches :gap-opens :query-start :query-end :subject-start :subject-end :evalue :bitscore :qlen :slen :species :version]
            (conj (mapv str (clojure.string/split line #"\t")) (:species opts) (:version opts)))))
  
(defn parse-reader 
  "Parse BLAST+ results from a reader. Should be lazy."
  [opts rdr]
  (for [entry (line-seq rdr)]
    (-parse opts entry)))
