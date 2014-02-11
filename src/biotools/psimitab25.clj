(ns biotools.psimitab25
  (:require clojure.java.io
            clojure.string
            [clojure.core.reducers :as r]
            [iota :as iota]
            [clojure-csv.core :as csv]))

(defrecord PSIMITAB25 [a-interactor-id 
                       b-interactor-id 
                       a-alt-id 
                       b-alt-id 
                       a-aliases 
                       b-aliases 
                       detection-methods
                       author
                       publication-id
                       a-taxonomy
                       b-taxonomy
                       interaction-types
                       source-databases
                       interaction-identifiers
                       confidence-score])

(defn ^:private into-map
  [raw]
  (if (= raw "-")
    {}
    (apply 
      hash-map
      (flatten
        (map
          (fn [x]
            (clojure.string/split x #":" 2))
          (mapv clojure.string/trim 
                (clojure.string/split raw #"\|")))))))

(defn- ^:private -parse
  [line]
    (let [
          [a-interactor-id 
           b-interactor-id 
           a-alt-id 
           b-alt-id 
           a-aliases 
           b-aliases 
           detection-methods
           author
           publication-id
           a-taxonomy
           b-taxonomy
           interaction-types
           source-databases
           interaction-identifiers
           confidence-score]
          (mapv clojure.string/trim (clojure.string/split line #"\t"))]
      
      (->PSIMITAB25
        (into-map a-interactor-id)
        (into-map b-interactor-id)
        (into-map a-alt-id)
        (into-map b-alt-id)
        (into-map a-aliases)
        (into-map b-aliases)
        (into-map detection-methods)
        author
        (into-map publication-id)
        (into-map a-taxonomy)
        (into-map b-taxonomy)
        (into-map interaction-types)
        (into-map source-databases)
        (into-map interaction-identifiers)
        confidence-score)))

; add drop 1
(defn parse-lazy
  "Lazy parser. Slower method, given a reader parses the lines."
  ([rdr]
    (for [line (line-seq rdr)]
      (-parse line))))

(defn parse-r
  "Returns reducer rather than foldcat'd version"
  ([filename]
    (->> 
      (iota/vec filename)
      (r/drop 1)
      (r/map -parse)
      (r/foldcat))))

(defn parse
  "Not lazy, but handles big files I believe. Faster parser using reducers. Only need to pass the filename, not a reader"
  ([filename]
    (r/foldcat (parse-r filename))))

