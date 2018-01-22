(ns ja-morph-diff.test-diff
  (:require [clojure.test :refer :all]
            [ja-morph-diff.diff :refer :all]
            [ja-morph-diff.parse :refer :all]
            [ja-morph-diff.main :refer :all]
            [clj-mecab.parse :as mecab]
            [clojure.math.combinatorics :as combo]))

(deftest test-morpheme-comparison
  (seq (compare-morphemes (first (mecab/parse-sentence "きょう")) (first (mecab/parse-sentence "今日")))))

(deftest test-sentence-comparison
  (seq (compare-morphemes-seq (mecab/parse-sentence "きょうは") (mecab/parse-sentence "今日は"))))

(deftest test-summarize-chunking
  (for [example ["おとうさんに、そういっておくのだよ。」"
                 "WithblissIwouldyieldmybreath."
                 "こんにちは、じゃがな"
                 "こんにちは、こんにちは"
                 "　　　　　　　――"
                 "（僕ハコンナヿヲ彼女ニ知ラセナイ方ガヨイカモ知レナイ。"]]
    (for [[[a a-morphemes] [b b-morphemes]] (combo/combinations (multi-dic-parse [:unidic-cwj #_:unidic-csj :unidic-kindai] comparing-features example) 2)
          :let [a-orths (mapv :mecab.features/orth a-morphemes)
                b-orths (mapv :mecab.features/orth b-morphemes)
                chunking-diff (summarize-chunking a-orths b-orths (compare-chunking a-orths b-orths))
                a-morphemes-aligned (if (seq chunking-diff)
                                      (:annotated-diff (annotated-diff chunking-diff a-morphemes :replacement/from-span))
                                      a-morphemes)
                b-morphemes-aligned (if (seq chunking-diff)
                                      (:annotated-diff (annotated-diff chunking-diff b-morphemes :replacement/to-span))
                                      b-morphemes)]]
      {:a a-orths
       :b b-orths
       :a-a a-morphemes-aligned
       :b-a b-morphemes-aligned
       :d chunking-diff
       :m-d (compare-morphemes-seq a-morphemes-aligned b-morphemes-aligned)})))
