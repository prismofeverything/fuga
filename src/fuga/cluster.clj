(ns fuga.cluster)

(defn closest
  [point means distance]
  (first (sort-by #(distance % point) means)))

(defn point-groups
  [means data distance]
  (group-by #(closest % means distance) data))

(defn new-means
  [average point-groups means]
  (for [old means]
    (if (contains? point-groups old)
      (average (get point-groups old))
      old)))

(defn iterate-means
  [data distance average]
  (fn [means]
    (let [next-means (new-means average (point-groups means data distance) means)]
      (println next-means)
      next-means)))

(defn groups
  [data distance means]
  (vals (point-groups means data distance)))

(defn take-while-unstable 
  ([sq]
     (lazy-seq
      (if-let [sq (seq sq)]
        (cons (first sq) (take-while-unstable (rest sq) (first sq))))))

  ([sq last]
     (lazy-seq
      (if-let [sq (seq sq)]
        (if (= (first sq) last) '() (take-while-unstable sq))))))

;; We can make a function, which takes our data, notion of distance, and notion of average,
;; and gives us back a function which, for a given set of initial guesses at the means,
;; shows us how the group memberships change.
(defn k-groups
  [data distance average]
  (fn [guesses]
    (take-while-unstable
     (map
      (partial groups data distance)
      (iterate (iterate-means data distance average) guesses)))))
