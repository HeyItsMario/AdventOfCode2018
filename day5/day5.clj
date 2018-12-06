(ns day5)


(def input
  (let [polymer (slurp "./input.txt")]
    (clojure.string/replace polymer "\n" "")))

#_(def input "dabAcCaCBAcCcaDA")
#_(def input "bAAacCbe")

(defn find-next-unit
  ""
  [polymer]
  (loop [polymer polymer
         matches #{}]
    (if (or (not (empty? matches)) (nil? (seq polymer)))
      matches
      (let [units (re-find #"(?i)([a-z])\1" polymer)
            units (first units) 
            matches (if (= (first units) (second units))
                      matches
                      (conj matches units))]
        (recur (second (clojure.string/split polymer (re-pattern (str (first units))) 2)) 
               matches)))))

(defn find-filter-remove
  ""
  [input]
  (loop [poly input
         unit-to-be-removed (find-next-unit poly)]
    (if (= 0 (count unit-to-be-removed))
      poly
      (let [result (clojure.string/replace poly (first unit-to-be-removed) "")]
        (recur result
               (find-next-unit result))))))

(defn part-one-l
  ""
  [input]
  (count (find-filter-remove input)))

(def part-one
  (memoize part-one-l))

(defn part-two
  ""
  []
  (let [alphabet (map char (range 97 123))
        polymer input]
    (pmap (fn [letter]
                    (let [stripped-polymer (clojure.string/replace polymer
                                                                   (re-pattern (str "(?i)" letter))
                                                                   "")]
                      (part-one stripped-polymer)))
                  alphabet)))
