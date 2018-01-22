(ns ja-morph-diff.diff
  (:require [clojure.spec.alpha :as s]
            [orchestra.core :refer [defn-spec]]

            [clojure.string :as string]
            [clojure.set :as set]

            [clj-mecab.parse :as mecab]

            [clojure.math.combinatorics :as combo]
            [plumula.diff.spec :as pdiff-spec]
            [plumula.diff :as pdiff]
            [clojure.data :as data]
            [diffit.vec :as vd]
            [diffit.map :as md]))

(s/def :string/diff-ops
  (s/coll-of (s/keys :plumula.diff/operation :plumula.diff/text)))

(defn-spec colorize-string-diff any?
  [ops :string/diff-ops]
  (into [:p]
        (for [op ops
              :let [o (:plumula.diff/operation op)
                    t (:plumula.diff/text op)]]
          (case o
            :plumula.diff/equal t
            :plumula.diff/insert [:span.insert t]
            :plumula.diff/delete [:span.delete t]))))

(defn-spec compare-strings (s/coll-of (s/keys :plumula.diff/operation :plumula.diff/text))
  [a-str string?
   b-str string?]
  (pdiff/diff a-str b-str :pdiff-spec/cleanup :piff-spec/cleanup-semantic))

(s/def :feature.diff/from (s/map-of keyword? string?))
(s/def :feature.diff/to (s/map-of keyword? string?))
(s/def :replacement/from-span (s/tuple int? int?))
(s/def :replacement/to-span (s/tuple int? int?))
(s/def :replacement/from (s/coll-of string?))
(s/def :replacement/to (s/coll-of string?))
(s/def :replacement/frequency int?)
(s/def :replacement/comparing-pos (s/coll-of [:mecab.features/pos-1 :mecab.features/pos-2]))
(s/def :replacement/pos-diff (s/cat :from :replacement/comparing-pos :to :replacement/comparing-pos))
(s/def :replacement/unit-diff (s/cat :from int? :to int?))
(s/def :replacement/type (s/keys :req [:replacement/pos-diff :replacement/unit-diff]))
#_(s/def :results/replacement
    (s/keys :req [:replacement/from
                  :replacement/to
                  :replacement/frequency]))
(s/def :results/replacements
  (s/keys :req [[:replacement/from :replacement/to] :results/frequency]))
(s/def :results/replacement-types
  (s/keys :req [:replacement/type :results/replacements :results/frequency]))

(s/def :comparison/from :mecab/dictionary)
(s/def :comparison/to :mecab/dictionary)
(s/def :dictionary/comparison
  (s/keys :req [:comparison/from :comparison/to]))
(s/def :stats/morpheme-counts (s/tuple int? int?))
(s/def :stats/morpheme-differences ratio?)
(s/def :stats/chunking-differences ratio?)
(s/def :results/stats
  (s/keys :req [:stats/morpheme-counts :stats/morpheme-differences :stats/chunking-differences]))
(s/def :dictionary/summary
  (s/keys :req [:results/replacement-types :results/stats]))
(s/def :results/summary
  (s/map-of :dictionary/comparison :dictionary/summary))

;; Functions

(defn-spec compare-morphemes
  (s/nilable
   (s/map-of keyword? any? #_:mecab/features #_(s/map-of #{:features/from :features/to} string?)))
  [a :mecab/morpheme
   b :mecab/morpheme]
  (if (not= a b)
    (let [ks (set (keys a))]
      (reduce
       (fn [m k]
         (if (= (k a) (k b))
           (assoc m k (k a))
           (assoc m k #:features{:from (k a) :to (k b)})))
       {}
       ks))))

(defn-spec compare-morphemes-seq any?
  [a-morphemes (s/coll-of :mecab/morpheme)
   b-morphemes (s/coll-of :mecab/morpheme)]
  (loop [as a-morphemes
         bs b-morphemes
         comp-seq []]
    (if (or (not (seq as)) (not (seq bs)))
      comp-seq
      (let [a (first as)
            b (first bs)]
        (cond
          (and (= :ignore a) (= :ignore b))
          (recur (next as) (next bs) comp-seq)

          (= :ignore a)
          (recur (next as) bs comp-seq)

          (= :ignore b)
          (recur as (next bs) comp-seq)

          :else
          (let [diffs (compare-morphemes a b)
                diff? (seq diffs)]
            (recur (next as) (next as)
                   (conj comp-seq (if diff? diffs)))))))))

(defn-spec compare-chunking any?
  [a-chunks (s/coll-of string?)
   b-chunks (s/coll-of string?)]
  (second (vd/diff a-chunks b-chunks)))

(defn apply-op [op morphemes idx replacement]
  (case op
    :+ (vec (vd/insert-at morphemes idx replacement))
    :- (vec (vd/remove-at morphemes idx replacement))))

(defn-spec summarize-chunking
  (s/coll-of
   (s/keys :replacement/from :replacement/to
           :replacement/from-span :replacement/to-span)) ;;:results/summary
  [a-chunks (s/coll-of string?)
   b-chunks (s/coll-of string?)
   ops (s/coll-of (s/cat :op #{:- :+ :=} :idx int? :replacement any?))]
  ;; Operations must come in +/- pairs for chunking diffs; this does
  ;; not necessarily hold for morpheme feature differences.
  ;; We know that from:to must match when their orth is joined!
  (:results
   (reduce
    (fn [{:keys [results chunks from to from-span to-span offset] :as m}
         {:keys [op idx replacement]}]
      (let [patched-chunks (apply-op op chunks idx replacement)

            {:keys [results chunks from to from-span to-span] :as m}
            (if (= :+ op)
              (-> m
                  (assoc :to replacement)
                  (assoc :to-span
                         [idx
                          (+ idx (count replacement))])
                  (update :offset + (count replacement)))
              (-> m
                  (assoc :from (subvec chunks idx (+ idx replacement)))
                  (assoc :from-span
                         [(- idx offset)
                          (- (+ idx replacement) offset)])
                  (update :offset - replacement)))]
        (if (not= (string/join from) (string/join to))
          (assoc m :chunks patched-chunks)
          (merge (update m :results
                         conj {:replacement/from from
                               :replacement/from-span from-span
                               :replacement/to to
                               :replacement/to-span to-span})
                 {:chunks patched-chunks
                  :from nil :to nil :from-span nil :to-span nil}))))
    {:results   []
     :chunks    a-chunks
     :from      nil
     :to        nil
     :from-span nil
     :to-span   nil
     :offset    0}
    (s/conform (s/coll-of (s/cat :op #{:- :+ :=} :idx int? :replacement any?)) ops))))

(defn annotated-diff
  [chunking-diff morphemes selector]
  (reduce (fn [{:keys [annotated-diff offset] :as a} [start end]]
            (let [start (- start offset)
                  end (- end offset)
                  offset (Math/abs (- end start))]
              (-> a
                  (assoc :annotated-diff
                         (vec (concat (subvec annotated-diff 0 start)
                                      (vec (repeat offset :ignore))
                                      (subvec annotated-diff end)))))))
          {:annotated-diff morphemes
           :offset 0}
          (->> chunking-diff (mapv selector))))

(defn-spec compare-results any?
  [dictionary-parse-map any? #_(s/map-of :mecab/dictionary (s/coll-of (s/map-of keyword? string?)))]
  (for [[[a a-morphemes] [b b-morphemes]] (combo/combinations dictionary-parse-map 2)
        :let [a-count (count a-morphemes)
              b-count (count b-morphemes)
              a-orths (mapv :mecab.features/orth a-morphemes)
              b-orths (mapv :mecab.features/orth b-morphemes)
              chunking-diff (summarize-chunking a-orths b-orths (compare-chunking a-orths b-orths))

              a-morphemes-aligned (if (seq chunking-diff)
                                    (:annotated-diff (annotated-diff chunking-diff a-morphemes :replacement/from-span))
                                    a-morphemes)
              b-morphemes-aligned (if (seq chunking-diff)
                                    (:annotated-diff (annotated-diff chunking-diff b-morphemes :replacement/to-span))
                                    b-morphemes)

              morpheme-diff (compare-morphemes-seq a-morphemes-aligned b-morphemes-aligned)]]
    {:comparison/from a
     :comparison/to b
     :sentence/from a-orths
     :sentence/to b-orths
     :stats/morpheme-count [a-count b-count]
     :stats/morpheme-differences (count (filter identity morpheme-diff)) ;; FIXME from matching/aligned chunks, count number that have different features values
     :stats/chunking-differences [(->> chunking-diff (map :replacement/from) (map count) (reduce +))
                                  (->> chunking-diff (map :replacement/to) (map count) (reduce +))]
     :chunking/diff chunking-diff
     :morpheme/diff morpheme-diff}))
