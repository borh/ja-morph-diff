(require '[clojure.java.shell :as sh])

(defn next-version [version]
  (when version
    (let [[a b] (next (re-matches #"(.*?)([\d]+)" version))]
      (when (and a b)
        (str a (inc (Long/parseLong b)))))))

(defn deduce-version-from-git
  "Avoid another decade of pointless, unnecessary and error-prone
  fiddling with version labels in source code."
  []
  (let [[version commits hash dirty?]
        (next (re-matches #"(.*?)-(.*?)-(.*?)(-dirty)?\n"
                          (:out (sh/sh "git" "describe" "--dirty" "--long" "--tags" "--match" "[0-9].*"))))]
    (try
      (cond
        dirty? (str (next-version version) "-" hash "-dirty")
        (pos? (Long/parseLong commits)) (str (next-version version) "-" hash)
        :otherwise version)
      (catch Exception e (println "Not a git repository or empty repository. Please git init in this directory/make a commit.")))))

(def project "ja-morph-diff")
(def version (deduce-version-from-git))

(set-env! :resource-paths #{"src"}
          :source-paths   #{"src" "test"}
          :dependencies   '[[org.clojure/clojure "1.9.0" :scope "provided"]

                            [adzerk/boot-test "RELEASE" :scope "test"]
                            [adzerk/bootlaces "0.1.13" :scope "test"]
                            [org.clojure/test.check "0.10.0-alpha2" :scope "test"]
                            [spec-provider "0.4.11" :scope "test"]
                            [orchestra "2017.11.12-1" :scope "test"]

                            [org.clojure/tools.cli "0.3.5"]
                            [me.raynes/fs "1.4.6"]
                            [clj-mecab "0.4.18-g98fc213-dirty"]
                            [corpus-utils "0.2.10-gfb277dc-dirty"]

                            [clj-time "0.14.2"]
                            [hiccup "2.0.0-alpha1"]
                            [garden "1.3.3"]

                            [kixi/stats "0.4.0"]
                            [redux "0.1.4"]
                            [org.clojure/math.combinatorics "0.1.4"]
                            [plumula/diff "0.1.1"]
                            [diffit "1.0.0"]])

(task-options!
 pom {:project     (symbol project)
      :version     version
      :description "Japanese language morpheme analysis results comparator"
      :url         "https://github.com/borh/ja-morph-diff"
      :scm         {:url "https://github.com/borh/ja-morph-diff"}
      :license     {"Eclipse Public License"
                    "http://www.eclipse.org/legal/epl-v10.html"
                    "BSD"
                    "BSD"}}
 aot {:all true}
 jar {:main 'ja-morph-diff.main :file (str project "-" version ".jar")}
 ;;sift {:include #{#"ja-morph-diff"}}
 target {:dir #{"target"}})

(require '[adzerk.bootlaces :refer :all])
(require '[ja-morph-diff.main])

(bootlaces! version)

(deftask build
  "Build and install the project locally."
  []
  (comp (pom) (jar) (target) (install)))

(deftask dev
  []
  (comp (watch) (build) (repl :init-ns 'ja-morph-diff.main :server true)))

(deftask uberjar []
  (comp
   (aot)
   (pom)
   (uber)
   (jar)
   (target)))

(deftask run
  []
  (ja-morph-diff.main/-main ""))

(require '[adzerk.boot-test :refer [test]])
