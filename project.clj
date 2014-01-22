(defproject crypto-challenge "0.1.0-SNAPSHOT"
  :description "Solutions to the Matasano Crypto Challenge"
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [org.clojure/data.codec "0.1.0"]
                 [org.clojure/data.priority-map "0.0.4"]]
  :main ^:skip-aot crypto-challenge.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
