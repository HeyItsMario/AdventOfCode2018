(ns day4)

(def input
  (let [lines (slurp "./input.txt")]
    (clojure.string/split lines #"\n")))


(defn map-input
  "Turns the vector into a map."
  []
  (map (fn [line]
         (let [guard-badge (re-find #"#\d+" line)]
           {:date (re-find #"\d+-\d+-\d+" line)
            :time (re-find #"\d\d:\d\d" line)
            :verb (if guard-badge
                    :begin-shift
                    (if (re-find #"wakes" line)
                      :wakes-up
                      :falls-asleep))
            :guard guard-badge})) input))


(defn sort-input
  "sort the map by date and then by time"
  []
  (sort-by (juxt :date :time) (map-input)))

