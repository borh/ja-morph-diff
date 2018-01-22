(ns ja-morph-diff.main
  (:require [clojure.spec.alpha :as s]
            [clojure.string :as string]
            [orchestra.spec.test :as st] ;; TODO
            [orchestra.core :refer [defn-spec]]
            [spec-provider.provider :as sp]

            [clojure.tools.cli :refer [parse-opts]]
            [clj-mecab.parse :as mecab]

            [ja-morph-diff.report :as report]))

(def dictionaries
  [:unidic :unidic-kindai :unidic-csj :unidic-cwj])

(def comparing-features
  #{:mecab.features/pos-1 :mecab.features/pos-2
    :mecab.features/c-type :mecab.features/c-form
    :mecab.features/lemma :mecab.features/orth})

(def cli-options
  [["-l" "--list" "List available dictionaries"]
   ["-d" "--dictionary DICTIONARY_NAME" "Use dictionary for comparison"
    :default nil #_(:dictionary/default mecab/dictionaries-info)
    :parse-fn (fn [s] (keyword s))
    :assoc-fn (fn [m k v] (update m k conj v))
    :validate [(fn [v] (mecab/valid-dictionaries v)) (format "Dictionary must be one of: %s" (string/join ", " mecab/valid-dictionaries))]]
   ["-o" "--out OUTPUT_FILE" "Set output file."]
   ["-f" "--feature FEATURE" (format "Compare morpheme feature. Must be one of: %s" (string/join ", " (map name [:mecab.features/pos-1 :mecab.features/pos-2 :mecab.features/pos-3 :mecab.features/pos-4 :mecab.features/c-type :mecab.features/c-form :mecab.features/l-form :mecab.features/lemma :mecab.features/orth :mecab.features/pron :mecab.features/orth-base :mecab.features/pron-base :mecab.features/goshu :mecab.features/i-type :mecab.features/i-form :mecab.features/f-type :mecab.features/f-form :mecab.features/i-con-type :mecab.features/f-con-type :mecab.features/type :mecab.features/kana :mecab.features/kana-base :mecab.features/form :mecab.features/form-base :mecab.features/a-type :mecab.features/a-con-type :mecab.features/a-mod-type :mecab.features/lid :mecab.features/lemma-id])))]
   ["-v" nil "Verbosity level"
    :id :verbosity
    :default 0
    :assoc-fn (fn [m k _] (update-in m [k] inc))]
   ["-h" "--help"]])

(defn -main [& args]
  (let [{:keys [options arguments summary]} (parse-opts args cli-options)
        options (cond-> options
                  (nil? (:dictionary options)) (assoc :dictionary dictionaries))]
    (when (:help options)
      (println summary)
      (System/exit 0))
    (println options)
    (when (pos? (:verbosity options))
      (st/instrument))))

(st/instrument)
