(ns aoc-2022.core
  (:require [clj-http.client :as client]
            [clojure.string :as str]
            [clojure.java.io :as io]))

(def credentials (str/trim (slurp "credentials.txt")))

(defn get-remote-input
  [year day]
  (-> (str "https://adventofcode.com/" year "/day/" day "/input")
      (client/get {:cookies {"session" {:value credentials}}})
      :body))

(defn get-puzzle-input
  [year day]
  (let [f (io/file "input" (str year) (format "%02d.txt" day))]
    (if (.exists f)
      (slurp f)
      (let [remote-input (get-remote-input year day)]
        (io/make-parents f)
        (spit f remote-input)
        remote-input))))

(defn run-solution
  [{:keys [year day part-1 part-2]}]
  (let [input (get-puzzle-input year day)
        result-1 (part-1 input)
        result-2 (part-2 input)]
    (println "Part 1: " result-1)
    (println "Part 2: " result-2)))
