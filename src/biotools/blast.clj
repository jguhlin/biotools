(ns biotools.blast
    (:require clojure.java.io
            clojure.string
            [clojure.core.reducers :as r]
            [iota :as iota]))

(defrecord BlastHit [query-id subject-id pct-identity alignment-length mismatches gap-opens query-start query-end subject-start subject-end evalue bitscore query-length subject-length query-alignment-percent subject-alignment-percent])

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
                (float (* 100 (/ (Integer/parseInt alignment-length) (Integer/parseInt slen))))
                )))

(defn parse-reader 
  "Parse BLAST+ results from a reader. Should be lazy. Now accepts minimum %ID and minimum alignment length as a percentage.
   Percent alignment is based off of query length to the alignment-length, as a percentage. A 211nt alignment where 211 align
   from the blast results are 100% aligned."
  ([rdr]
    (for [entry (line-seq rdr)]
      (-parse entry)))
  ([pct-id-min query-align-min rdr]
    (keep (fn [x] (if (and (>= (:pct-identity x) pct-id-min) (>= (:query-alignment-percent x) query-align-min)) x nil)) 
          (for [entry (line-seq rdr)]
            (-parse entry))))
  ([pct-id-min query-align-min subject-align-min rdr]
    (keep (fn [x] (if (and (>= (:pct-identity x) pct-id-min) 
                           (>= (:query-alignment-percent x) query-align-min)
                           (>= (:subject-alignment-percent x) subject-align-min))
                    x nil)) 
          (for [entry (line-seq rdr)]
            (-parse entry)))))

(defn -create-filter-fn 
  [pct-id-min query-align-min subject-align-min]
  (fn [x]
    (if
      (and
        (>= (:pct-identity x) pct-id-min)
        (>= (:query-alignment-percent x) query-align-min)
        (>= (:subject-alignment-percent x) subject-align-min))
      x
      nil)))

(defn parse-reader-via-reducer
  "Parse BLAST+ results from a filename. Not lazy, but can exceed memory. Also accepts minimum %ID and minimum alignment length as a percentage.
   Percent alignment is based off of query length to the alignment-length, as a percentage. A 211nt alignment where 211 align
   from the blast results are 100% aligned."
  ([pct-id-min query-align-min subject-align-min filename]
    (let [filter-fn (-create-filter-fn pct-id-min query-align-min subject-align-min)]
      (->> (iota/vec filename)
        (r/map -parse)
        (r/filter filter-fn)
        (r/foldcat))))
  ([filename]
      (->> (iota/vec filename)
        (r/map -parse)
        (r/foldcat)))
  )

(defn parse-reader-as-reducer
  "Parse BLAST+ results from a filename. Not lazy, but can exceed memory. Also accepts minimum %ID and minimum alignment length as a percentage.
   Percent alignment is based off of query length to the alignment-length, as a percentage. A 211nt alignment where 211 align
   from the blast results are 100% aligned."
  ([pct-id-min query-align-min subject-align-min filename]
    (let [filter-fn (-create-filter-fn pct-id-min query-align-min subject-align-min)]
      (->> (iota/vec filename)
        (r/map -parse)
        (r/filter filter-fn))))
  ([filename]
      (->> (iota/vec filename)
        (r/map -parse))))



