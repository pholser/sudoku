(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(def all-values #{1 2 3 4 5 6 7 8 9})

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (not= 0 (value-at board coord)))

(defn row-values [board [row _]]
  (set (value-at board [row])))

(defn col-values [board [_ col]]
  (set (map #(nth % col) board)))

(defn coord-pairs [coords]
  (for [row coords col coords] [row col]))

(defn top-left-of-block-of [coord]
  (mapv (fn [point] (some #(if (= 0 (rem % 3)) %) (range point -1 -1)))
        coord))

(defn block-values [board coord]
  (let [[r c] (top-left-of-block-of coord)]
    (reduce
      clojure.set/union
        (map
          #(col-values (subvec board r (+ r 3)) [r %])
          (range c (+ c 3))))))

(defn valid-values-for [board coord]
  (if (has-value? board coord)
    #{}
    (reduce
      clojure.set/difference
      all-values
      [(row-values board coord)
       (col-values board coord)
       (block-values board coord)])))

(defn filled? [board]
  (boolean (every? #(has-value? board %) (coord-pairs (range 9)))))

(defn rows [board]
  (map #(row-values board [% 0]) (range 9)))

(defn valid-rows? [board]
  (every? #(= all-values %) (rows board)))

(defn cols [board]
  (map #(col-values board [0 %]) (range 9)))

(defn valid-cols? [board]
  (every? #(= all-values %) (cols board)))

(defn blocks [board]
  (map #(block-values board %) (for [r (range 0 9 3) c (range 0 9 3)] [r c])))

(defn valid-blocks? [board]
  (every? #(= all-values %) (blocks board)))

(defn valid-solution? [board]
  ((every-pred valid-rows? valid-cols? valid-blocks?) board))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  (first (filter (complement (partial has-value? board)) (coord-pairs (range 9)))))

;; Recap of backtracking:
;
;    check if you are at the end
;    if so, is the solution valid?
;        if not, return an empty sequence
;        otherwise return [solution]
;    if not
;        select an empty location
;        try solving with each valid value for that location

(defn solve [board]
  (if (filled? board)
    (if (valid-solution? board) board [])
    (let [empty-point (find-empty-point board)
          candidates (valid-values-for board empty-point)]
      (for [e candidates
            soln (solve (set-value-at board empty-point e))]
        soln))))