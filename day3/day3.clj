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
    {:tl coords
     :tr [(+ wide (first coords)) (second coords)]
     :bl [(first coords) (+ tall (second coords))]
     :br [(+ wide (first coords)) (+ tall (second coords))]}))

(defn point-in-sheets ""
  [sheet point corner]
  (let [x (first point)
        y (second point)
        left-edge (first (sheet :tl))
        right-edge (first (sheet :tr))
        top-edge (second (sheet :tl))
        bottom-edge (second (sheet :bl))]
    (if (and (< x right-edge)
             (> x left-edge)
             (> y top-edge)
             (< y bottom-edge))
      {:corner corner
       :coords [x y]}
      false)))

(defn sheet-overlap
  "Returns a list of corners in sheet-b that are inside sheet-a"
  [sheet-a sheet-b]
  (remove false? (map (fn [[corner point]]
                      (point-in-sheets sheet-a point corner)) sheet-b)))

#_(defmulti one-corner-overlap
  (fn [sheet corners]
    (into [] (map (fn [points] (:corner points)) corners))))

(defmulti two-corner-overlap)
(defmulti three-corner-overlap)
(def four-corner-overlap)

(def opposite-corners
  {:tr :bl
   :tl :br
   :br :tl
   :bl :tr})

(defn one-corner-overlap
  "Returns the area that is made if only one corner is inside"
  [sheet corners]
  (let [opposite-corner (opposite-corners ((first corners) :corner))
        opp-coords (sheet opposite-corner)
        curr-coords ((first corners) :coords)
        y (- (second curr-coords) (second opp-coords))
        x (- (first curr-coords) (first opp-coords))]
    (if (neg? (* x y))
      (* -1 (* x y))
      (* x y))))

(defn two-corner-overlap
  ""
  [sheet corners]
  (let [inside-corners (into [] corners)
        first-corner-c ((first corners) :coords)
        second-corner-c ((second corners) :coords)
        ]))


