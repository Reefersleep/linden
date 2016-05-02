(ns linden.core)

(defn- append
  "Returns coll with the appropriate value appended"
  [rules coll key]
  (if-not (contains? rules key)
    (conj coll key)
    (let [val (get rules key)]
      (if (fn? val)
        (into coll (val))
        (into coll val)))))

(defn next-generation
  [rules initiator]
  (let [informed-append (partial append rules)]
    (reduce informed-append [] initiator)))

(defn generations
  "Returns a lazy list of l-system generations"
  [rules initiator]
  (let [informed-next-generation (partial next-generation rules)]
    (iterate informed-next-generation initiator)))
