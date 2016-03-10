(ns l-mayer.core-test
  (:require [clojure.test :refer :all]
            [l-mayer.core :refer :all]))

;;example usage
;;(nth-generation {[1 2 3] [[1 2 3] ["and" "a" "b" "c"]]} [[1 2 3]] 1)
;;=> [[1 2 3] ["and" "a" "b" "c"]]

;;Leaf tests

(defn function-1 [] (+ 1 2))

(defn function-2 [] "What a fantastic function")

(deftest leaves
  (are [a b] (= [a b] (nth-generation {a [a b]} [a] 1))
    (comment "You can use keywords as leaves")
    :a :b

    (comment "You can use strings as leaves")
    "a" "b"

    (comment "You can use integers as leaves")
    9 5

    (comment "You can use fractions as leaves")
    1/2 20/21

    (comment "You can use vectors as leaves")
    [1 2 3] ["and" "a" "b" "c"]

    (comment "You can use sets as leaves")
    #{1 2 3} #{"and" "a" "b" "c"}

    (comment "You can use maps as leaves")
    {:name "John" :pet "Cuddlesnaps"} {:name "Olivia" :pet "Snakey the horse"}

    (comment "You can use functions as leaves")
    function-1 function-2))

(defn a-or-b [] (rand-nth [:a :b]))

(defn one-to-four [] (rand-nth [1 2 3 4]))

(deftest sprouts
  (comment "If you use a function instead of a leaf
            (let's call it a sprout),
            the return value of the function will become a leaf.
            These are simple examples for demonstration.")
  (are [sprout result] (= [result] (nth-generation {:rule sprout} [:rule] 1))
    (fn [] :a) :a
    (fn [] :b) :b)
  (comment "Actually, the option to use a sprout was
           was put in to enable side-effectful randomness,
           as this allows for less rigid, more organic
           tree drawing")
  (let [result1 (first (nth-generation {:rule a-or-b} [:rule] 1))]
    (is (or (= result1 :a)
            (= result1 :b))))
  (let [result2 (first (nth-generation {:rule one-to-four} [:rule] 1))]
    (is (or (= result2 1)
            (= result2 2)
            (= result2 3)
            (= result2 4)))))
 
