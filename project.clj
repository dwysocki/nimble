(defproject nimble "0.1.0-SNAPSHOT"
  :description "An expert system for playing Nim, written in Clojure."
  :url "https://github.com/dwysocki/nimble"
  :license {:name "MIT License"
            :url "http://opensource.org/licenses/MIT"}
  :dependencies [[org.clojure/clojure "1.7.0"]]
  :main ^:skip-aot nimble.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
