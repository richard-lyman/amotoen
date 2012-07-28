(defproject com.lithinos/amotoen "0.2.0-SNAPSHOT"
    :description "Amotoen is a Clojure library that supports PEG style definitions of grammars that can produce parsers."
    :url "http://www.lithinos.com/amotoen"
    :license {:name "EPL-v1.0" :distribution :repo :comments "same as Clojure" :url "http://www.eclipse.org/legal/epl-v10.html"}
    :source-path "src"
    :resource-paths ["res"]
    :dependencies [[org.clojure/clojure "1.4.0"]]
    :jar-name "amotoen.jar"
    :jar-exclusions [#"(markdown|minimark|project).clj" #"maven"]
    :manifest {"Built-By" "\"Richard Lyman\" <richard.lyman@gmail.com>"})
