(ns day2)

(def input
    (let [lines (slurp "./input.txt")]
      (clojure.string/split lines #"\n")))

(defn match-two ""
  []
  (reduce (fn [acc ids]
            (if (contains? (set ids) 2)
              (+ acc 1)
              (+ acc 0)))
          0
          (map (fn [id]
                 (map (fn [mtch]
                        (count (first mtch)))
                      (re-seq #"([a-z])\1*"
                              (clojure.string/join #"" (sort id))))) input)))


(defn match-three ""
  []
  (reduce (fn [acc ids]
            (if (contains? (set ids) 3)
              (+ acc 1)
              (+ acc 0)))
          0
          (map (fn [id]
                 (map (fn [mtch]
                        (count (first mtch)))
                      (re-seq #"([a-z])\1\1*"
                              (clojure.string/join #"" (sort id))))) input)))


(defn diffrnc ""
  []
  )
