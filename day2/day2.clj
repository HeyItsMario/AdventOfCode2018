(ns day2)

(def input
    (let [lines (slurp "./input.txt")]
      (clojure.string/split lines #"\n")))

#_(def input ["bcbc" "abcd" "abba" "cdef" "abcg"])

(def alph-num
  {"a" 1
   "b" 2
   "c" 3
   "d" 4
   "e" 5
   "f" 6
   "g" 7
   "h" 8
   "i" 9
   "j" 10
   "k" 11
   "l" 12
   "m" 13
   "n" 14
   "o" 15
   "p" 16
   "q" 17
   "r" 18
   "s" 19
   "t" 20
   "u" 21
   "v" 22
   "w" 23
   "x" 24
   "y" 25
   "z" 26})

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


(defn get-input-as-num-seq ""
  []
  (map (fn [id]
         (-> (fn [char]
               (alph-num char))
             (map (clojure.string/split id #"")))) input))


(defn subtract-lists ""
  [first-list left-overs]
  (let [neg-first-list (map (fn [num] (* -1 num)) first-list)]
    (map (fn [nums]
           (map + nums neg-first-list)) left-overs)))


(defn find-match ""
  [num-list]
  (loop [first-l (first num-list)
         rest-l (rest num-list)
         index-f 0]
    (let [sbl (subtract-lists first-l rest-l)
          freqs (map (fn [lst] (frequencies lst)) sbl)
          mtch (keep-indexed (fn [indx f-vec]
                               (if (= ((into (sorted-map) f-vec) 0) 3)
                                 indx)) (into [] freqs))]
      (if (seq mtch)
        {:a index-f
         :b mtch}
        (recur (first rest-l)
               (rest rest-l)
               (inc index-f))))))
