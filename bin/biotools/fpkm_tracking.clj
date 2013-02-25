(ns biotools.fpkm-tracking)

(defrecord Condition [FPKM conf_lo conf_high status])
(defrecord FPKMTracking [tracking_id class_code nearest_ref_id gene_id gene_short_name tss_id locus length coverage conditions])

(defn ^:private -handle-conditions
  [FPKM conf_lo conf_hi status]
  )

(defn ^:private -parse-conditions
  "Parse the header line of the Cufflinks genes.fpkm_tracking and return it as a vector of each of the
expression conditions. Expression conditions are typically tissues but may also be other types"
  [line]
    (let [
          [_ _ _ _ _ _ _ _ _ & the-conditions] 
          (mapv clojure.string/trim (clojure.string/split line #"\t"))]
      (mapv (comp first #(clojure.string/split (first %) #"_")) (partition 1 4 the-conditions))))

(defn ^:private -parse
  [conditions line]
    (let [
          [tracking_id class_code nearest_ref_id gene_id 
           gene_short_name tss_id locus length coverage 
           & the-conditions] 
          (mapv clojure.string/trim (clojure.string/split line #"\t"))]
      
      (->FPKMTracking
        tracking_id
        class_code
        nearest_ref_id
        gene_id
        gene_short_name
        tss_id
        locus
        length
        coverage
        (doall (map hash-map conditions (map (partial apply ->Condition) (partition 4 the-conditions)))))))

(defn parse-reader 
  ([rdr]
    (let [line-seq-reader (line-seq rdr)
          conditions (-parse-conditions (first line-seq-reader))
          lines (rest line-seq-reader)]
      (for [line lines]
        (-parse conditions line)))))