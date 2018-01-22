(ns ja-morph-diff.test-main
  (:require [clojure.test :refer :all]
            [ja-morph-diff.main :refer :all]
            [ja-morph-diff.diff :refer :all]
            [ja-morph-diff.report :refer :all]
            [ja-morph-diff.parse :refer :all]))

(deftest test-string-comparison
  (let [string-comparisons (compare-strings "漢字" "漢じ")]
    (write! "test-string-comparison.html" (make-report (colorize-string-diff string-comparisons)))))

(deftest test-aozora-report
  (let [aozora-path (clojure.java.io/file "/home/bor/Projects/aozora-corpus-generator/Corpora/Japanese-Aozora-Bunko-and-Kawabata-2017-12-26/Plain/")]
    (doseq [comparison (parse-corpus aozora-path [:unidic-cwj :unidic-csj :unidic :unidic-kindai] comparing-features)]
      (write-report! comparison))))

(deftest test-hssrpc-report
  (let [hssrpc-path (clojure.java.io/file "/home/bor/Projects/hssrpc/pdfs/")]
    (doseq [comparison (parse-corpus hssrpc-path [:unidic-cwj :unidic-csj :unidic] comparing-features)]
      (write-report! comparison))))
