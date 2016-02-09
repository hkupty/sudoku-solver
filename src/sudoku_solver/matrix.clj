(ns sudoku-solver.matrix
  (:require [clojure.core.matrix :refer :all]
            [clojure.core.matrix.operators :refer :all]
  ))


; The game constists of 9 grids of 9 grids
(def game
  (atom
    (matrix [
             [ ( matrix [[0 0 0] [6 8 0] [1 9 0]] ) ( matrix [[2 6 0] [0 7 0] [0 0 4]] ) ( matrix [[7 0 1] [0 9 0] [5 0 0]] ) ]
             [ ( matrix [[8 2 0] [0 0 4] [0 5 0]] ) ( matrix [[1 0 0] [6 0 2] [0 0 3]] ) ( matrix [[0 4 0] [9 0 0] [0 2 8]] ) ]
             [ ( matrix [[0 0 9] [0 4 0] [7 0 3]] ) ( matrix [[3 0 0] [0 5 0] [0 1 8]] ) ( matrix [[0 7 4] [0 3 6] [0 0 0]] ) ]
             ])))
(def possible-items #{1 2 3 4 5 6 7 8 9})

(defn get-as-set [f x y]
  "Return as set"
  (set (flatten (map (fn [r] (f r y)) (f @game x)))) )

(defn get-row-as-set [x y]
  "Return row as set"
  (get-as-set get-row x y))

(defn get-column-as-set [x y]
  "Return column as set"
  (get-as-set get-column x y))

(defn get-matrix-as-set [x y]
  "Return matrix as set"
  (set (flatten ((@game x) y))))

(defn get-used-for-cell [i j x y]
  "Return all used numbers for column, row and matrix"
  (clojure.set/union (get-matrix-as-set i j) (get-column-as-set j y) (get-row-as-set i x)))

(defn get-possible-solution-for [i j x y]
  "Return all possible solutions for coord"
  (let [curr (get-value-for i j x y)]
    (if-not (= curr 0)
      #{curr}
      (clojure.set/difference possible-items (get-used-for-cell i j x y)))))

(defn has-definite-solution? [i j x y]
  "Check if a sqare has only a single available solution"
  (= (count (get-possible-solution-for i j x y)) 1))

(defn get-value-for [i j x y]
  "Retur value for coordinate"
  ((((@game i) j) x) y))

(defn is-cell-solved? [i j x y]
  "Is cell correctly valued"
  (not= (get-value-for i j x y) 0))

(defn solve-cell! [i j x y]
  "Attempt to solve cell"
  (let [curr (get-value-for i j x y)]
    (if-not (= curr 0)
      curr
      (let [pos (get-possible-solution-for i j x y)]
        (if (= (count pos) 1)
          (set-value! i j x y (first pos))
          nil
          )))))

(defn set-value! [i j x y v]
  "Set value for inner node"
    (swap! game mset i j x y v))
