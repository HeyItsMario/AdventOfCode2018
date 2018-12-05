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

(defn create-shift-data
  "Turns the vector into a map."
  []
  (->> (reductions (fn [prev-map line]
                    {:date (line :date)
                     :time (line :time)
                     :verb (line :verb)
                     :guard (or (line :guard) (prev-map :guard))}) {} (sort-input))
       (remove empty?)))



(defn group-by-badge
  ""
  []
  (group-by :guard (create-shift-data)))

(defn get-total-sleep
  ""
  [guard-data]
  (let [fell-asleep (map (fn [data]
                           (-> (clojure.string/replace (re-find #":\d\d" (data :time)) ":" "")
                               (Integer.)))
                         (filter #(= (% :verb) :falls-asleep)
                                 guard-data))
        wakes-up (map (fn [data]
                        (-> (clojure.string/replace (re-find #":\d\d" (data :time)) ":" "")
                            (Integer.)))
                      (filter #(= (% :verb) :wakes-up)
                              guard-data))]
    (reduce + 0 (map - wakes-up fell-asleep))))

(defn get-most-mins
  ""
  [guard-data]
  (let [fell-asleep (map (fn [data]
                           (-> (clojure.string/replace (re-find #":\d\d" (data :time)) ":" "")
                               (Integer.)))
                         (filter #(= (% :verb) :falls-asleep)
                                 guard-data))
        wakes-up (map (fn [data]
                        (-> (clojure.string/replace (re-find #":\d\d" (data :time)) ":" "")
                            (Integer.)))
                      (filter #(= (% :verb) :wakes-up)
                              guard-data))
        result (frequencies (flatten (map (fn [start end]
                                            (range start end)) fell-asleep wakes-up)))]
    (first (into (sorted-map-by (fn [k1 k2]
                                          (compare [(get result k2) k2]
                                                   [(get result k1) k1])))
                         result))))

(defn guard-with-most
  ""
  []
  (first (sort-by :total > 
                  (map (fn [[k v]]
                         {:guard k
                          :total (get-total-sleep v)})
                       (group-by-badge)))))

(defn guard-with-most-same
  ""
  []
  (last (sort-by :amount 
                 (map (fn[[k v]]
                        {:min (first (get-most-mins v))
                         :amount (second (get-most-mins v))
                         :guard k})
                      (group-by-badge)))))

(defn get-part-1
  ""
  []
  (let [badge (-> (guard-with-most)
                  :guard)
        badge-int (Integer. (clojure.string/replace badge "#" ""))]
    (* badge-int (first (get-most-mins ((group-by-badge) badge))))))

(defn get-part-2
  ""
  []
  (let [guard (guard-with-most-same)
        min (guard :min)
        badge (Integer. (clojure.string/replace (guard :guard) "#" ""))]
    (* min badge))) 
