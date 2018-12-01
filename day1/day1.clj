(ns day1)


(def freqs
  (let [lines (slurp "./input.txt")]
    (clojure.string/split lines #"\n")))


(reduce (fn [acc char]
          (+ (Integer. char) acc)) 0 freqs)
