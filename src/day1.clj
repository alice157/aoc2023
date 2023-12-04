(ns day1
  (:require
    [clojure.java.io :as io]
    [clojure.pprint :as pprint]
    [clojure.string :as str]))


(def lines
  (->  (io/resource "day1.txt")
       (slurp)
       (str/split-lines)))


(def digit-char->int
  (zipmap (map char (range (int \0) (inc (int \9))))
          (range 0 (inc 9))))


(defn calibration-number
  "Calibration number (sum of first and last digits) for the given string."
  [s]
  (let [digits (vec (keep digit-char->int s))]
    (parse-long (str (first digits)
                     (last digits)))))


(defn part1
  [lines]
  (transduce (map calibration-number) + 0 lines))


(def digit->int
  (merge (zipmap (map str (range 0 10))
                 (range 0 10))
         {"zero" 0
          "one" 1
          "two" 2
          "three" 3
          "four" 4
          "five" 5
          "six" 6
          "seven" 7
          "eight" 8
          "nine" 9}))


(def digit-regex
  (re-pattern
    (str "^("
         (str/join "|" (keys digit->int))
         ")")))


(defn digits-2
  [s]
  (into []
        (comp (take-while some?)
              (keep #(first (re-find digit-regex %)))
              (map digit->int))
        (iterate #(when-not (str/blank? %)
                    (subs % 1))
                 s)))


(defn calibration-number-2
  [s]
  (let [digits (digits-2 s)]
    (parse-long (str (first digits)
                     (last digits)))))


(defn part2
  [lines]
  (transduce (map calibration-number-2) + 0 lines))


(comment
  (part1 lines)
  (part2 lines))
