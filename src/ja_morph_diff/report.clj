(ns ja-morph-diff.report
  (:require [clojure.spec.alpha :as s]
            [orchestra.core :refer [defn-spec]]

            [clojure.string :as string]
            [orchestra.spec.test :as st] ;; TODO

            [clj-mecab.parse :as mecab]

            [clj-time.local :as l]
            [clj-time.format :as f]

            [hiccup2.core :refer [html]]
            [hiccup.page :refer [html5]]
            [garden.core :refer [css]]))

(comment
  (defn render-sentence-diff [d]
    [:div.sentence-diff
     ]))

(defn render-stats [{:keys [total summary]}]
  [:div.stats
   [:table
    (into [:thead [:td "Frequency"]] (map (fn [k] [:td k]) (keys summary)))
    (into
     [:tbody
      (into [:tr
             [:td total]]
            (map (fn [v] [:td (format "%.2f" v)]) (vals summary)))])]])

(defn render-transform-freqs [from to xs]
  [:div.transforms
   [:table
    [:thead [:td from] [:td to] [:td "Frequency"]]
    (->> xs
         (sort-by second >)
         (reduce
          (fn [a [[[from to] & r] v]]
            (conj a [:tr
                     (into [:td] (map (fn [x] [:span.morpheme x]) from))
                     (into [:td] (map (fn [x] [:span.morpheme x]) to))
                     [:td v]]))
          [:tbody]))]])

(defn render-morpheme-freqs [from to xs]
  (let [feature-keys (sort (keys (ffirst (drop-while #(string? (second %)) xs))))]
    [:div.transforms
     [:p [:span.delete from] " → " [:span.insert to]]
     [:table
      (conj
       (into [:thead]
             (map (fn [k] [:td (name k)]) feature-keys))
       [:td "Frequency"])
      (->> xs
           (sort-by second >)
           (reduce
            (fn [a [features v]]
              (conj a (conj (into [:tr]
                                  (map
                                   (fn [feature-name]
                                     (let [feature-diff (feature-name features)]
                                       [:td (if (string? feature-diff)
                                              [:span.same feature-diff]
                                              [:span
                                               [:span.delete (:features/from feature-diff)]
                                               "→"
                                               [:span.insert (:features/to feature-diff)]])]))
                                   feature-keys))
                            [:td v])))
            [:tbody]))]]))

(defn-spec make-report any?
  [body any?]
  (html5
   {:lang "ja" :encoding "UTF-8"}
   [:head
    [:meta {:charset "utf-8"}]
    [:meta {:http-equiv "X-UA-Compatible" :content "IE=edge,chrome=1"}]
    [:meta {:name "viewport" :content "width=device-width, initial-scale=1.0, user-scalable=no"}]
    [:style (string/join
             "\n"
             [#_(css [:p {:writing-mode "tb-rl" :font-weight 600}])
              (css [:div.box {:border "1px solid black" :margin "4px" :padding "4px"}])
              (css [:h1 :h2 :h3 :h4 :h5 :h6 [:&:after {:content "' '"
                                                       :display "block"
                                                       :border "2px solid black"}]])
              (css [:div.transforms {:display "inline-flex"
                                     :flex-wrap "nowrap"
                                     :align-items "center"
                                     :justify-content "center"
                                     :flex-direction "column"
                                     ;;:justify-content "center"
                                     }])
              (css [:ul.morpheme-list {:list-style "none" :display "flex"}])
              (css [:li.morpheme ])
              (css [:span.morpheme {#_:outline #_"2px solid black" :border "1px solid black" :margin "2px" #_:width #_"100px"}])
              (css [:span.chunk {:border "2px dotted black" :margin "2px"}])
              (css [:span.same {:color "grey"}])
              (css [:span.insert {:color "blue"
                                  :text-decoration "underline"
                                  #_:text-decoration "underline"
                                  #_:text-decoration-color "blue"}])
              (css [:span.delete {:color "red"
                                  :text-decoration "underline"
                                  #_:text-decoration "line-through"
                                  ;;:text-decoration-style "double"
                                  #_:text-decoration-color "red"}])])]]
   body))

(defn-spec write! nil?
  ([filename string?
    content any?]
   (spit filename content))
  ([from-dic any? #_:mecab/dictionary
    to-dic any? #_:mecab/dictionary
    content any?]
   (write! (format "%s-%s-comparison-%s.html"
                   (name from-dic)
                   (name to-dic)
                   (f/unparse (f/formatter "yyyy-MM-dd") (l/local-now)))
           content)))

(defn prettify-symbol [s]
  (string/join " " (map string/capitalize (mapcat #(string/split % #"[\.\-]") (filter identity ((juxt namespace name) s))))))

(defn write-report! [{:keys [comparison/from comparison/to data summary]}]
  (let [from-dic (prettify-symbol from)
        to-dic (prettify-symbol to)]
    (write!
     (string/replace from-dic " " "_")
     (string/replace to-dic " " "_")
     (make-report
      [:div.report
       [:h2 (format "Comparison of %s and %s" from-dic to-dic)]
       (let [{:keys [corpus.document.summary/morpheme-difference-stats
                     corpus.document.summary/morpheme-counts
                     corpus.document.summary/chunk-difference-counts-from
                     corpus.document.summary/chunk-difference-counts-to
                     corpus.document.summary/chunk-difference-details
                     corpus.document.summary/morpheme-difference-details]} summary]
         [:div.corpus-summary
          [:div.box
           [:h3 (prettify-symbol 'corpus.document.summary/morpheme-difference-stats)]
           [:p from-dic ": " (first morpheme-counts) " morphemes"]
           [:p to-dic ": " (second morpheme-counts) " morphemes"]
           (render-stats morpheme-difference-stats)]
          [:div.box
           [:h3 (prettify-symbol 'corpus.document.summary/chunk-difference-counts-from) " " from-dic]
           (render-stats chunk-difference-counts-from)]
          [:div.box
           [:h3 (prettify-symbol 'corpus.document.summary/chunk-difference-counts-to) " " to-dic]
           (render-stats chunk-difference-counts-to)]
          [:div.box
           [:h3 (prettify-symbol 'corpus.document.summary/chunk-difference-details)]
           (render-transform-freqs from-dic to-dic chunk-difference-details)]
          [:div.box
           [:h3 (prettify-symbol 'corpus.document.summary/morpheme-difference-details)]
           (render-morpheme-freqs from-dic to-dic morpheme-difference-details)]])
       (into [:div]
             (map (fn [{:keys [file diffs summary]}]
                    (let [{:keys [document.summary/morpheme-difference-stats
                                  document.summary/morpheme-counts
                                  document.summary/chunk-difference-counts-from
                                  document.summary/chunk-difference-counts-to
                                  document.summary/chunk-difference-details
                                  document.summary/morpheme-difference-details]}
                          summary]
                      [:div.file-summary
                       [:div.box
                        [:h3 file]
                        [:div.box
                         [:h4 (prettify-symbol 'document.summary/morpheme-difference-stats)]
                         [:p from-dic ": " (first morpheme-counts) " morphemes"]
                         [:p to-dic ": " (second morpheme-counts) " morphemes"]
                         (render-stats morpheme-difference-stats)]
                        [:div.box
                         [:h4 (prettify-symbol 'document.summary/chunk-difference-counts-from) " " from-dic]
                         (render-stats chunk-difference-counts-from)]
                        [:div.box
                         [:h4 (prettify-symbol 'document.summary/chunk-difference-counts-to) " " to-dic]
                         (render-stats chunk-difference-counts-to)]
                        [:div.box
                         [:h4 (prettify-symbol 'document.summary/chunk-difference-details)]
                         (render-transform-freqs from-dic to-dic chunk-difference-details)]
                        [:div.box
                         [:h4 (prettify-symbol 'differences/morpheme-difference-details)]
                         (render-morpheme-freqs from-dic to-dic morpheme-difference-details)]]]))
                  data))]))))
