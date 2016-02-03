(ns l-mayer.core)

(defn a [arg] (inc arg))

(defn b [arg] (inc (inc arg)))

(def rules {a [a b]
            b [a]})


(defn substitute
  [rules initiator]
  (vec (flatten (map #(if-not (contains? rules %) [%]
                              (let [value (get rules %)]
                                (if (clojure.test/function? value) (value)
                                    value)))
                     initiator))))

(defn l-system
  "Takes an 'initiator' in the form of a vector containing
  zero, one or more of the elements used in the 'rules' map.
  Each key in 'rules' must have a vector value containing
  zero, one or more elements. 'iterations' should be an
  integer. l-system will return a seq where each of the
  elements in 'initiator' has been recursively replaced
  - for the number of times described with 'iterations' -
  with the element's corresponding value in 'rules'.
  Context-free."
  [initiator rules iterations]
  (if (= 0 iterations) initiator
      (l-system (substitute rules initiator)
                rules
                (dec iterations))))

(defn either
  [arg1 arg2]
  (let [random (rand-int 2)]
    (condp = random
      0 arg1
      1 arg2)))

(defn one-of
  [& args]
  (let [random (rand-int (count args))]
    (get args random)))

(defn X
  "Nothing"
  [state] state) ;;X

(defn L
  "Nothing"
  [state] state) ;;L

(defn F
  "Draw line"
  [state] state) ;;F

(defn <'
  "Turn"
  [state] state) ;;+

(defn >'
  "Inverse turn"
  [state] state) ;;-

(defn S
  "Save current state"
  [state] state) ;;[

(defn R
  "Restore latest saved state"
  [state] state) ;;]

(def X1 [F S >' X F <' X <' X R])

(def X2 [F S <' X F >' X >' X R])

(def rules {F [F L]
            L [F]
            X #(either X1 X2)})

(def start [X])

(def l-product (l-system start rules 7))

;; Not L-system, only relates to drawing the tree
(defn execute [l-product initial] (eval (cons '->
                                              (cons initial
                                                    l-product))))

(defn execute2 [l-product initial] ((apply comp (reverse l-product)) initial))

;; (execute (l-system [a] rules 2))
