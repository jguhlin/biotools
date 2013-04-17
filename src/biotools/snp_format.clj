(ns biotools.snp-format
  (:require [clojure.string :as string]
            [clojure.core.reducers :as r]
            [iota :as iota]))

(defrecord SNPdata [position gene-context ref-allele MAF multi A-count C-count G-count T-count accessions])

; The reads files in the format of the Medicago HapMap project SNP format.
; This returns a foldable collection, which should become the standard for biotools package

(defn ^:private -parse
  [accession-identifiers line]
    (let [
          [position gene-context ref-allele MAF multi A-count C-count G-count T-count & accessions]
            (mapv clojure.string/trim (clojure.string/split line #"\t"))]
      (->SNPdata
        (Integer/parseInt position)
        gene-context
        ref-allele
        (Double/parseDouble MAF)
        (Integer/parseInt multi)
        (Integer/parseInt A-count)
        (Integer/parseInt C-count)
        (Integer/parseInt G-count)
        (Integer/parseInt T-count)
        (zipmap accession-identifiers accessions)
        )))

(defn ^:private -parse-header
  [line]
  (drop 9 (mapv clojure.string/trim (clojure.string/split line #"\t"))))

(defn filter-reference-alleles
  "Remove accessions whose SNP matches the reference alleles"
  [snpdata]
  (merge snpdata 
         {:accessions (into {} (filter #(not (= (:ref-allele snpdata) (val %))) (:accessions snpdata))) }))

(defn parse
  "Takes a filename and parses the file. Returns a reducible, (hopefully) foldable collection."
  ([filename]
    (let [fv (iota/vec filename) ; File vector, so we can automate the processing of this...
          accessions (-parse-header (second fv))
          lines (drop 2 fv)]
      (r/map (partial -parse accessions) lines))))

