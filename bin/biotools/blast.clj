(ns biotools.blast)

(defrecord BlastHit [query-id subject-id pct-identity alignment-length mismatches gap-opens query-start query-end subject-start subject-end evalue bitscore query-length subject-length query-alignment-percent])

(defn ^:private -parse
  [line]
  (let [[query-id subject-id pct-identity alignment-length mismatches gap-opens query-start query-end 
         subject-start subject-end evalue bitscore qlen slen 
         ]  (mapv clojure.string/trim (clojure.string/split line #"\t"))]
    (->BlastHit (str query-id) 
                (str subject-id)
                (Float/parseFloat pct-identity)
                (Integer/parseInt alignment-length)
                (Integer/parseInt mismatches)
                (Integer/parseInt gap-opens)
                (Integer/parseInt query-start)
                (Integer/parseInt query-end)
                (Integer/parseInt subject-start)
                (Integer/parseInt subject-end)
                (Float/parseFloat evalue)
                (Float/parseFloat bitscore)
                (Integer/parseInt qlen)
                (Integer/parseInt slen)
                (float (* 100 (/ (Integer/parseInt alignment-length) (Integer/parseInt qlen))))
                )))

(defn parse-reader 
  "Parse BLAST+ results from a reader. Should be lazy. Now accepts minimum %ID and minimum alignment length as a percentage.
   Percent alignment is based off of query length to the alignment-length, as a percentage. A 211nt alignment where 211 align
   from the blast results are 100% aligned. Keep best option is in the translator/database part."
  ([opts rdr]
  (for [entry (line-seq rdr)]
    (-parse opts entry)))
  ([opts pct-id-min pct-align-min rdr]
    (keep (fn [x] (if (and (>= (:pct-identity x) (Float/parseFloat pct-id-min)) (>= (:query-alignment-percent x) (Float/parseFloat pct-align-min))) x nil)) 
                    (for [entry (line-seq rdr)]
                      (-parse entry)))))
