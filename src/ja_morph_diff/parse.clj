(ns ja-morph-diff.parse
  (:require [clojure.spec.alpha :as s]
            [orchestra.core :refer [defn-spec]]

            [ja-morph-diff.diff :as diff]
            [ja-morph-diff.stats :as stats]
            [ja-morph-diff.report :as report]

            [clojure.string :as string]
            [me.raynes.fs :as fs]
            [corpus-utils.text :as text]
            [clj-mecab.parse :as mecab]))

(defn-spec multi-dic-parse
  (s/map-of :mecab/dictionary (s/coll-of (s/map-of keyword? string?)))
  [dics (s/coll-of :mecab/dictionary)
   comparing-features (s/coll-of keyword?)
   sentence string?]
  (reduce
   (fn [a dic]
     (assoc a dic
            (mapv (fn [m]
                    (cond-> (select-keys m comparing-features)
                      (:mecab.features/lemma m)
                      (update :mecab.features/lemma
                              (fn [s] (string/replace s #"-.+" "")))))
                  (mecab/with-dictionary dic (mecab/parse-sentence sentence)))))
   {}
   dics))

(defn file? [f] (instance? java.io.File f))

(defn-spec parse-document any?
  [selected-dics (s/coll-of :mecab/dictionary)
   comparing-features (s/coll-of keyword?)
   filename string?]
  (sequence
   (comp (mapcat text/split-japanese-sentence)
         (map (partial multi-dic-parse selected-dics comparing-features)))
   (string/split-lines (slurp filename))))

(defn-spec parse-corpus any?
  [corpus-dir file?
   selected-dics (s/coll-of :mecab/dictionary)
   comparing-features (s/coll-of keyword?)]
  (let [files (fs/glob corpus-dir "*.txt")
        pair-data (->> files
                       (pmap (fn [file]
                               (let [basename (fs/base-name file)
                                     parsed-doc (parse-document selected-dics comparing-features file)
                                     pair-diffs (group-by (juxt :comparison/from :comparison/to) (mapcat diff/compare-results parsed-doc))]
                                 (for [[dic-pair diffs] pair-diffs]
                                   {:comparison/from (first dic-pair)
                                    :comparison/to (second dic-pair)
                                    :file basename
                                    :diffs diffs
                                    :summary (stats/document-summary diffs)}))))
                       (flatten)
                       (group-by (juxt :comparison/from :comparison/to)))]
    (for [[dic-pair data] pair-data]
      {:comparison/from (first dic-pair)
       :comparison/to (second dic-pair)
       :data data
       :summary (stats/corpus-summary data)})))
