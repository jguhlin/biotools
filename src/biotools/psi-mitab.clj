(ns biotools.psi-mitab25
  (:require [clojure.string :as string]))

;https://code.google.com/p/psicquic/wiki/MITAB25Format

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
