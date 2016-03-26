(ns linden.core)

;; Core

(defn append-replacement
  [key rules coll]
  (if-not (contains? rules key)
    (conj coll key)
    (let [val (get rules key)]
      (if (fn? val)
        (conj coll (val))
        (into coll val)))))

(defn replace'
  ([rules initiator]
   (replace' rules initiator []))
  ([rules initiator result]
   (if (empty? initiator)
     result
     (let [current-key (first initiator)
           new-result (append-replacement current-key
                                          rules
                                          result)]
       (replace' rules
                 (rest initiator)
                 new-result)))))

(defn generations
  "Returns a lazy list of l-system generations"
  [rules initiator]
  (let [informed-replace' (partial replace' rules)]
    (iterate informed-replace' initiator)))

(defn nth-generation
  [rules initiator n]
  (nth (generations rules initiator) n))

;; Demo - Tree drawing

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
            X #(rand-nth [X1 X2])})

(def start [X])

(def l-product (nth-generation rules start 7))

;; Drawing the tree
(defn execute [l-product initial] (eval (cons '->
                                              (cons initial
                                                    l-product))))

(defn execute2 [l-product initial] ((apply comp (reverse l-product)) initial))
