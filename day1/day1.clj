(ns day1)


(def freqs
  (let [lines (slurp "./input.txt")]
    (map #(Integer. %) (clojure.string/split lines #"\n"))))


(def count-fr
  (count freqs))

(defn rdc ""
  []
  (reduce (fn [acc num]
            (+ acc num)) 0 freqs))


(defn rpt ""
  []
  (loop [acc 0
         tke 0
         my-s #{0}]
    (let [res (+ acc (nth freqs tke))]
      (if (contains? my-s res)
        res
        (recur res
               (mod (inc tke) count-fr)
               (conj my-s res))))))
