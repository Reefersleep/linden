(ns linden.core)

(defn- append-replacement
  "Returns coll with the appropriate value appended"
  [key rules coll]
  (if-not (contains? rules key)
    (conj coll key)
    (let [val (get rules key)]
      (if (fn? val)
        (into coll (val))
        (into coll val)))))

(defn- substitute
  [rules initiator]
  (loop [initiator initiator
         result []]
    (if (empty? initiator)
      result
      (recur (rest initiator)
             (append-replacement (first initiator)
                                 rules
                                 result)))))

(defn generations
  "Returns a lazy list of l-system generations"
  [rules initiator]
  (let [informed-substitute (partial substitute rules)]
    (iterate informed-substitute initiator)))
