(defproject crypto-experiments "0.1.0-SNAPSHOT"
  :description "Experiments in cryptography with Clojure"
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [org.clojure/data.codec "0.1.0"]
                 [org.clojure/data.priority-map "0.0.4"]]
  :main ^:skip-aot crypto-experiments.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
