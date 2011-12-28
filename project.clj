(defproject com.lithinos/amotoen "0.1.0-SNAPSHOT"
    :description "Amotoen is a Clojure library that supports PEG style definitions of grammars that can produce parsers."
    :license {  :name "EPL-v1.0"
                :url "http://www.eclipse.org/legal/epl-v10.html"
                :distribution :repo
                :comments "same as Clojure"}  
    :dependencies [ [org.clojure/clojure "1.2.0"]
                    [org.clojure/clojure-contrib "1.2.0"]]
    :jar-name "amotoen.jar"
    :jar-exclusions [#"(errors|markdown|minimark|string_wrapper|utils|wrapper|csv|json|project).clj" #"maven"]
    :manifest {"Built-By" "\"Richard Lyman\" <richard.lyman@gmail.com>"}
                 )
