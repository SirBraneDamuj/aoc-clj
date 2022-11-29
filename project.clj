(defproject aoc-2022 "0.1.0-SNAPSHOT"
  :dependencies [[org.clojure/clojure "1.11.1"]
                 [clj-http "3.12.3"]]
  :profiles {:kaocha {:dependencies [[lambdaisland/kaocha "1.71.1119"]]}}
  :aliases {"kaocha" ["with-profile" "+kaocha" "run" "-m" "kaocha.runner"]}
  :repl-options {:init-ns aoc-2022.core})
