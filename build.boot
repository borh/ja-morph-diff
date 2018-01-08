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
          :source-paths   #{"test"}
          :dependencies   '[[org.clojure/clojure "1.9.0" :scope "provided"]

                            [adzerk/boot-test "RELEASE" :scope "test"]
                            [org.clojure/test.check "0.10.0-alpha2" :scope "test"]
                            [adzerk/bootlaces "0.1.13" :scope "test"]

                            [clj-mecab "0.4.16"]
                            [corpus-utils "0.2.9"]])

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
 aot {:namespace #{'ja-morph-diff.main}}
 jar {:main 'ja-morph-diff.main :file (str project "-" version ".jar")}
 target {:dir #{"target"}})

(require '[adzerk.bootlaces :refer :all])

(bootlaces! version)

(deftask build
  "Build and install the project locally."
  []
  (comp (pom) (jar) (target) (install)))

(deftask dev
  []
  (comp (watch) (build) (repl :init-ns 'ja-morph-diff.main :server true)))

(require '[adzerk.boot-test :refer [test]])
