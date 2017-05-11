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
  [d dist-method norm-method]
  (let [k (cl/knn-classifier d :dist-method dist-method :norm norm-method :k (count d))
        qk (fn [x] (-> (cons (first x) (-> (last x) first first vec sort)) vec))
        qc (fn [x y]
             (compare (-> (cons (first y) (drop 1 x)) vec)
                      (-> (cons (first x) (drop 1 y)) vec)))
        pm (pm/priority-map-keyfn-by qk qc)]
    (->> (map (fn [x]
                (let [p (pm/priority-map-by >)
                      nns (->> (rec/nearest-neighbours k x)
                               (filter #((complement #{(first x)}) (first %)))
                               (map (fn [[k v]] [(set [(first x) k]) v]))
                               (into p))
                      nn (first nns)]
                  [(second nn) (first x) nns]))
              d)
         (map (fn [c x] [c x]) (iterate inc 0))
         (into pm))))

(defn- next-neighbour
  [pm s]
  (let [ss (if (vector? s)
             (-> (flatten s) set)
             (set s))]
    (-> (drop-while (fn [[k v]] (every? ss k)) pm) first second)))

(defn- cluster
  [q]
  (loop [q q]
    (let [[x y & r] (seq q)]
      (if y
        (let [mn (merge-with #(max %1 %2) (-> (second x) last) (-> (second y) last))
              cl (with-meta [(-> (second y) second) (-> (second x) second)]
                   {:distance (-> (second x) first)})
              nq (conj (-> (pop q) pop) [(first x) [(next-neighbour mn cl) cl mn]])]
          (recur nq))
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
  normalisation (default :mod-standard-score)."
  [data & {:keys [dist-method norm-method]
           :or {dist-method :euclidean norm-method :mod-standard-score}}]
  (-> (init-queue data dist-method norm-method) cluster))

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

(defn- update-actives
  [a sep & hs]
  (swap! a #(reduce (fn [z y]
                      (if (z (+ y sep))
                        (dissoc z (+ y sep))
                        (assoc z (+ y sep) 1)))
                    %
                    hs)))

(defn- levels
  [a h]
  (->> (filter #(> % h) (keys @a))
       sort
       (reduce (fn [x y]
                 (let [a (reduce #(+ %1 (count %2)) 0 x)]
                   (conj x (str (string-n " " (- (- y (+ h a)) 1)) "|"))))
               [])
       (apply str)))

(defn- two-nodes?
  [z]
  (and (z/branch? z)
       (z/left z)
       (-> (z/left z) z/branch?)))

(defn- leaf-string
  [z h actives]
  (str (z/node z) " " (string-n "-" (- (- h 2) (count (z/node z))))
       "+" (levels actives h)))

(defn- leaf-print
  [z actives h sep]
  (let [bsh (- h (count-path z sep))
        ns (leaf-string z bsh actives)
        nl (str (string-n " " (- bsh 1)) "|" (string-n "-" (- sep 1)) "+" (levels actives (+ bsh sep)))]
    (cond (and (is-left? z)
               (not (is-next-branch? z)))
          (do (update-actives actives sep bsh) [ns nl])
          (and (is-left? z) (is-next-branch? z))
          (do (update-actives actives sep bsh (- bsh sep)) [ns nl])
          (and (not (is-left? z)) (z/branch? (z/left z)))
          (do (update-actives actives sep bsh)
              (update-actives actives sep (- bsh sep))
              [nl (leaf-string z bsh actives)])
          :else [ns])))

(defn- branch-print
  [z actives h sep]
  (let [bbh (- h (count-path z sep))]
    (when (z/left z)
      (when (two-nodes? z)
        (if-not (> (+ (* 2 sep) bbh) h) (update-actives actives sep (+ bbh sep)))
        [(str (string-n " " bbh) (string-n " " (- sep 1)) "|"
              (string-n "-" (- sep 1)) "+" (levels actives (+ bbh (* 2 sep))))]))))

(defn dendrogram
  "Returns a collection of strings representing a dendrogram of a
  binary tree."
  [coll & {:keys [sep] :or {sep 3}}]
  (let [z (z/vector-zip coll)
        h (max-length z sep)
        actives (atom {})]
    (mapcat #(if (z/branch? %)
               (branch-print % actives h sep)
               (leaf-print % actives h sep))
            (take-while (complement z/end?)
                        (iterate z/next z)))))

(defn print-dendrogram
  "Prints a dendrogram of a binary tree represented by a collection of
  vectors."
  [coll & {:keys [sep] :or {sep 3}}]
  (doseq [l (dendrogram coll :sep sep)]
    (println l)))

