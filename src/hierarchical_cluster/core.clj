(ns hierarchical-cluster.core
  (:require [clj-recommendation.core :as rec]
            [clj-classify.core :as cl]
            [clojure.data.priority-map :as pm]
            [clojure.zip :as z]
            [clojure.edn :as edn]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; cluster
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- init-queue
  [d dist-method norm-method type]
  (let [k (cl/knn-classifier d :dist-method dist-method :norm norm-method :k (count d))
        qk (fn [x] (-> (cons (first x) (-> (last x) first first sort)) vec))
        qc (fn [x y]
             (compare (-> (cons (first y) (drop 1 x)) vec)
                      (-> (cons (first x) (drop 1 y)) vec)))
        pm (pm/priority-map-keyfn-by qk qc)]
    (->> (map (fn [x]
                (let [p (pm/priority-map-by >)
                      nns (->> (rec/nearest-neighbours k x)
                               (filter #((complement #{(first x)}) (first %)))
                               (map (fn [[k v]] [[(first x) k] v]))
                               (into p))
                      nn (first nns)]
                  [(second nn) (first x) nns]))
              d)
         (map (fn [c x] [c x]) (iterate inc 0))
         (into pm))))

(defmulti cluster (fn [q t] t))

(defn- merge-pms
  ([pm1 pm2] (merge-pms pm1 pm2 >))
  ([pm1 pm2 f]
   (->> (seq (merge pm1 pm2))
        (group-by #(second (first %)))
        vals
        (filter #(> (count %) 1))
        (map #(sort-by second f %))
        (map first)
        (into (empty pm1)))))

(defmethod cluster :single
  [q t]
  (loop [q q]
    (let [[x y & r] (seq q)]
      (if y
        (let [mn (merge-pms (-> (second x) last) (-> (second y) last))
              cl (with-meta [(-> (second y) second) (-> (second x) second)]
                   {:distance (-> (second x) first)})]
          (recur (conj (-> (pop q) pop) [(first x) [(-> (peek mn) second) cl mn]])))
        (-> (drop 1 (second x)) first)))))

(defn- update-similarities
  [[index cluster sims] old-queue]
  (let [sim-set (-> cluster flatten set)
        nq (->> (pmap (fn [[entry-index [_ entry-cl entry-sims]]]
                        (let [e-cl-set (if (vector? entry-cl) (-> entry-cl flatten set) #{entry-cl})
                              gdist (->> (drop-while (fn [[k v]]
                                                       (not (e-cl-set (second k))))
                                                     (rseq sims))
                                         first)
                              new-sims (conj (->> (filter (fn [[k v]] (not (sim-set (second k))))
                                                          entry-sims)
                                                  (into (empty entry-sims)))
                                             [(-> (first gdist) reverse) (second gdist)])]
                          [[entry-index [(-> (peek new-sims) second) entry-cl new-sims]] gdist]))
                      old-queue))]
    (conj (->> (map first nq) (into (empty old-queue)))
          (let [n (->> (map second nq) (into (empty sims)))]
            [index [(-> (peek n) second) cluster n]]))))

(defmethod cluster :complete
  [q t]
  (loop [q q]
    (let [[x y & r] (seq q)]
      (if y
        (recur (update-similarities [(first x)
                                     (with-meta [(-> (second y) second) (-> (second x) second)]
                                       {:distance (-> (second x) first)})
                                     (merge (-> (second x) last) (-> (second y) last))]
                                    (-> (pop q) pop)))
        (-> (drop 1 (second x)) first)))))

(defn hierarchical-cluster
  "Performs hierarchical clustering and returns a nested vector of
  vectors representing a binary tree with distance values for each
  branch in the meta data of each vector. Data should be in the format
  of a collection of vectors, the first element being the class the
  second a vector of attribute values and the third a vector of
  comments. Distance method can be specified with the :dist-method
  keyword, allowed values are :euclidean, :manhattan, :cosine
  and :pearson (default :euclidean). Normalisation method can be
  specified using the :norm-method keyword, allowed values
  are :mod-standard-score, :standard-score, :min-max or false for no
  normalisation (default :mod-standard-score). Type of clustering can
  be specified with the :type keyword, allowed values are :single
  and :complete."
  [data & {:keys [dist-method norm-method type]
           :or {dist-method :euclidean norm-method :mod-standard-score
                type :single}}]
  (-> (init-queue data dist-method norm-method type) (cluster type)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; dendrogram
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- string-n
  [st n]
  (apply str (repeat n st)))

(defn- max-length
  [z sep]
  (->> (map #(+ (count (z/node %)) (* sep (count (z/path %))))
            (->> (take-while (complement z/end?) (iterate z/next z))
                 (filter (complement z/branch?))))
       (apply max)))

(defn- count-path
  [z sep]
  (if (z/branch? z)
    (* sep (count (z/path z)))
    (* sep (- (count (z/path z)) 1))))

(defn- is-left?
  [z]
  (z/right z))

(defn- is-next-branch?
  [z]
  (if-let [r (z/right z)]
    (z/branch? r)))

(defn- active-updater
  [h a sep]
  [(fn [& hs]
      (swap! a #(reduce (fn [z y]
                          (if-not (> (+ sep y) h)
                            (if (z (+ y sep))
                              (dissoc z (+ y sep))
                              (assoc z (+ y sep) 1))
                            z))
                        %
                        hs)))
   (fn [level]
     (->> (filter #(> % level) (keys @a))
          sort
          (reduce (fn [x y]
                    (let [a (reduce #(+ %1 (count %2)) 0 x)]
                      (conj x (str (string-n " " (- (- y (+ level a)) 1)) "|"))))
                  [])
          (apply str)))])

(defn- two-nodes?
  [z]
  (and (z/branch? z)
       (z/left z)
       (-> (z/left z) z/branch?)))

(defn- leaf-string
  [z h updater]
  (str (z/node z) " " (string-n "-" (- (- h 2) (count (z/node z))))
       "+" ((second updater) h)))

(defn- leaf-print
  [z updater h sep]
  (let [bsh (- h (count-path z sep))
        ns (leaf-string z bsh updater)
        nl (str (string-n " " (- bsh 1)) "|" (string-n "-" (- sep 1)) "+"
                ((second updater) (+ bsh sep)))]
    (cond (and (is-left? z)
               (not (is-next-branch? z)))
          (do ((first updater) bsh) [ns nl])
          (and (is-left? z) (is-next-branch? z))
          (do ((first updater) bsh (- bsh sep)) [ns nl])
          (and (not (is-left? z)) (z/branch? (z/left z)))
          (do ((first updater) bsh (- bsh sep))
              [nl (leaf-string z bsh updater)])
          :else [ns])))

(defn- branch-print
  [z updater h sep]
  (let [bbh (- h (count-path z sep))]
    (when (z/left z)
      (when (two-nodes? z)
        (if-not (>= (+ sep bbh) h) ((first updater) (+ bbh sep)))
        [(str (string-n " " bbh) (string-n " " (- sep 1)) "|"
              (string-n "-" (- sep 1)) "+" ((second updater) (+ bbh (* 2 sep))))]))))

(defn dendrogram
  "Returns a collection of strings representing a dendrogram of a
  binary tree."
  [coll & {:keys [sep] :or {sep 3}}]
  (let [z (z/vector-zip coll)
        h (max-length z sep)
        updater (active-updater h (atom {}) sep)]
    (mapcat #(if (z/branch? %)
               (branch-print % updater h sep)
               (leaf-print % updater h sep))
            (take-while (complement z/end?)
                        (iterate z/next z)))))

(defn print-dendrogram
  "Prints a dendrogram of a binary tree represented by a collection of
  vectors."
  [coll & {:keys [sep] :or {sep 3}}]
  (doseq [l (dendrogram coll :sep sep)]
    (println l)))

