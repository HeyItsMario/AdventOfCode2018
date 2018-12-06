(ns day5)


(def input
  (let [polymer (slurp "./input.txt")]
    (clojure.string/replace polymer "\n" "")))

#_(def input "dabAcCaCBAcCcaDA")

(defn part-one
  ""
  []
  (count (find-filter-remove)))

(defn find-filter-remove
  ""
  []
  (loop [poly input
         unit-to-be-removed (first (filter (fn [unit] (not (= (first unit) (second unit))))
                                           (map #(first %) (re-seq #"(?i)([a-z])\1"  poly))))]
    (if (nil? unit-to-be-removed)
      poly
      (let [result (clojure.string/replace-first poly unit-to-be-removed "")]
        (recur result
               (first (filter (fn [unit] (not (= (first unit) (second unit))))
                              (map #(first %) (re-seq #"(?i)([a-z])\1"  result)))))))))

