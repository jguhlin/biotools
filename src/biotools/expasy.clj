(ns biotools.expasy
  (:require [clojure.string :as string]))

(defrecord ENZYME [id definition alternate-name catalytic-activity cofactors comments prosite swiss-prot])

(def abbrev-to-record
  {"ID" :id
   "DE" :definition
   "AN" :alternate-name
   "CA" :catalytic-activity
   "CF" :cofactors
   "CC" :comments
   "PR" :prosite
   "DR" :swiss-prot})

(defn -parse-expasy-dat-file
  "Read the data from the given reader as a list of strings, where
each string is made up of multiple lines, separated by // on it's own
line."
  [reader]

  (partition 2
             (rest ; Skip the very first entry
               (partition-by #(re-find #"^ID\s+" (apply str %)) (line-seq reader)))))

(defn- -certain-records
  [line]
  (cond 
    (re-find #"^ID" line) true
    (re-find #"^DE" line) true
    (re-find #"^AN" line) false ; Remove, for now...
    (re-find #"^CF" line) true  ; Co-factor(s)
    (re-find #"^CC" line) true  ; Comments
    (re-find #"^PR" line) false ; Prosite, we have this from Interproscan
    (re-find #"^DR" line) false ; Swiss-Prot, not using...
    (re-find #"^//" line) false ; Separator...
    (re-find #"^CA" line) true  ; Catalytic activity
    :else true) ; Keep the rest, by default
  )

(defn- -combine-text
  [x & xs]
  (clojure.string/trim (clojure.string/join " " (into [x] xs))))

(defn- -split-element
  [element]
  (let [[type entry] (rest (re-matches #"(\w\w)\s+(.+)$" element))]
    {(get abbrev-to-record type) entry}))

; In a hurry so not dealing with prosite/swissprot at this time
(defn- -convert-to-record 
  [entry]
  (let [entrymap (apply merge-with -combine-text (map -split-element (filter -certain-records (map str entry))))]
    (map->ENZYME entrymap)))

(defn parse
  [rdr]
  (for [term-data (-parse-expasy-dat-file rdr)]
    (-convert-to-record (reverse (into (first term-data) (second term-data))))
      ))

