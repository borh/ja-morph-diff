(ns ja-morph-diff.stats
  (:require [clojure.spec.alpha :as s]
            [orchestra.core :refer [defn-spec]]

            [kixi.stats.core :as stats]
            [redux.core :refer [facet fuse]]))

(def sum
  "Calculates the sum of inputs."
  (fn
    ([] 0)
    ([n m] (+ n m))
    ([n] n)))

(def basic-stats
  (fuse {:summary stats/summary
         :total sum}))

(defn-spec document-summary any?
  [doc-diffs any?]
  #:document.summary
  {:morpheme-difference-stats (transduce (map :stats/morpheme-differences) basic-stats doc-diffs)
   :morpheme-counts (transduce (comp (map :stats/morpheme-count)) (facet sum [first second]) doc-diffs)
   :chunk-difference-counts-from (transduce (comp (map :stats/chunking-differences) (map first)) basic-stats doc-diffs)
   :chunk-difference-counts-to   (transduce (comp (map :stats/chunking-differences) (map second)) basic-stats doc-diffs)
   :chunk-difference-details (frequencies (sequence (comp (mapcat :chunking/diff) (filter identity) (map #(apply hash-map ((juxt :replacement/from :replacement/to) %)))) doc-diffs))
   :morpheme-difference-details (frequencies (sequence (comp (mapcat :morpheme/diff) (filter identity)) doc-diffs))})

(defn-spec corpus-summary any?
  [doc-summaries any? #_(s/coll-of (s/map-of keyword? any?))]
  #:corpus.document.summary
  {:morpheme-difference-stats (transduce (comp (mapcat :diffs) (map :stats/morpheme-differences)) basic-stats doc-summaries)
   :morpheme-counts (transduce (comp (mapcat :diffs) (map :stats/morpheme-count)) (facet sum [first second]) doc-summaries)
   :chunk-difference-counts-from (transduce (comp (mapcat :diffs) (map :stats/chunking-differences) (map first)) basic-stats doc-summaries)
   :chunk-difference-counts-to (transduce (comp (mapcat :diffs) (map :stats/chunking-differences) (map second)) basic-stats doc-summaries)
   :chunk-difference-details (frequencies (sequence (comp (mapcat :diffs) (mapcat :chunking/diff) (filter identity) (map #(apply hash-map ((juxt :replacement/from :replacement/to) %)))) doc-summaries))
   :morpheme-difference-details (frequencies (sequence (comp (mapcat :diffs) (mapcat :morpheme/diff) (filter identity)) doc-summaries))})
