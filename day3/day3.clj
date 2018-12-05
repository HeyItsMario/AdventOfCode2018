(ns day3)


(def input
  (let [lines (slurp "./input.txt")]
    (clojure.string/split lines #"\n")))


(defn make-sheet ""
  [claim]
  (let [claim (clojure.string/split (clojure.string/replace claim ":" "") #" ")
        dimensions (clojure.string/split (nth claim 3) #"x")
        wide (Integer. (first dimensions))
        tall (Integer. (second dimensions))
        coords (clojure.string/split (nth claim 2) #",")
        coords [(Integer. (first coords)) (Integer. (second coords))]]
    {:id (first claim)
     :x1 (first coords)
     :y1 (second coords)
     :x2 (+ wide (first coords))
     :y2 (+ tall (second coords))}))

(defn sheet-set ""
  []
  (let [my-set #{}]))
(defn calculate-intersect-area
  ""
  [sheet-a sheet-b]
  (let [union-area (* (max 0 (- (min (sheet-a :x2) (sheet-b :x2))
                                (max (sheet-a :x1) (sheet-b :x1))))
                      (max 0 (- (min (sheet-a :y2) (sheet-b :y2))
                                (max (sheet-a :y1) (sheet-b :y1)))))]
    union-area))




(defn test-run ""
  [sheets]
  (loop [main-sheet (first sheets)
         rest-sheet (rest sheets)
         counted #{(main-sheet :id)}
         acc 0]
    (let [result (map (fn [sheet]
                        {:sum (calculate-intersect-area main-sheet sheet)
                         :id (sheet :id)}) rest-sheet)
          flt (filter (fn [sheet]
                        (not (= 0 (sheet :sum)))) result)
          filter-counted (map (fn [sheet]
                                (if (not (contains? counted (sheet :id)))
                                  (sheet :id))) flt)
          r
          eal-sum (+ acc (reduce (fn [ac sheet]
                                    (if (not (contains? counted (sheet :id)))
                                      (+ ac (sheet :sum))
                                      (+ ac 0))) 0 flt))
          counted (apply conj counted filter-counted)]
      (if (= 1 (count (seq rest-sheet)))
        acc
        (recur (first rest-sheet)
               (rest rest-sheet)
               counted
               real-sum)))))




