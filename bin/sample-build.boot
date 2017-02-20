;; Set to be moved to some ultisnips package or.. elsewhere
;; in an easier to access mechanism

(set-env!
  :source-paths    #{"src/clj" "test/clj"}
  :dependencies '[[org.clojure/clojure "1.8.0"]])

(deftask build
  "Builds an uberjar of this project that can be run with java -jar"
  []
  (comp
    (aot :namespace '#{project.core})
    (pom :project 'project
         :version "0.0.0")
    (uber)
    (jar :main 'project.core)
    (target)))
