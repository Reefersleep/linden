(ns linden.core)

(defn append-replacement
  [key rules coll]
  (if-not (contains? rules key)
    (conj coll key)
    (let [val (get rules key)]
      (if (fn? val)
        (into coll (val))
        (into coll val)))))

(defn replace'
  ([rules initiator]
   (replace' rules initiator []))
  ([rules initiator result]
   (loop [rules rules
          initiator initiator
          result result]
     (if (empty? initiator)
       result
       (let [current-key (first initiator)
             new-result (append-replacement current-key
                                            rules
                                            result)]
         (recur rules
                (rest initiator)
                new-result))))))

(defn generations
  "Returns a lazy list of l-system generations"
  [rules initiator]
  (let [informed-replace' (partial replace' rules)]
    (iterate informed-replace' initiator)))
