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
        qc (fn [x y] (compare (-> (cons (first y) (drop 1 x)) vec)
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
    (->> (drop-while (fn [[k v]]
                       (try (every? ss k)
                            (catch Exception e
                              (println k)
                              (throw e))))
                     pm)
         first second)))

(defn- cluster
  [q]
;;  (println (take 2 q)) (println) (println)
  (loop [q q]
    (let [[x y & r] (seq q)]
      (if y
        (let [mn (merge-with #(max %1 %2) (-> (second x) last) (-> (second y) last))
              cl [(-> (second y) second) (-> (second x) second)]
              nq (conj (-> (pop q) pop) [(first x) [(next-neighbour mn cl) cl mn]])]
          ;; (doseq [[k v] nq]
          ;;   (println (first v) " " (second v)))
;;          (println (take 2 nq))(println) (println)
          (recur nq))
        (drop 1 (second x))))))

(defn hierarchical-cluster
  "Performs hierarchical clustering and returns a vector the first
  element of which is nested vector of vectors representing a binary
  tree and the second a hash-map of distances used generating the
  clusters. Data should be in the format of a collection of vectors,
  the first element being the class the second a vector of attribute
  values and the third a vector of comments. Distance method can be
  specified with the :dist-method keyword, allowed values
  are :euclidean, :manhattan, :cosine
  and :pearson (default :euclidean). Normalisation method can be
  specified using the :norm-method keyword, allowed values
  are :mod-standard-score, :standard-score, :min-max or false for no
  normalisation (default :mod-standard-score)."
  [data & {:keys [dist-method norm-method]
           :or {dist-method :euclidean norm-method :mod-standard-score}}]
  (let [pm (init-queue data dist-method norm-method)]
    (cluster pm)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; dendrogram
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- max-length
  [z]
  (loop [z z
         acc nil
         level 0]
    (if-not (z/end? z)
      (if (z/branch? z)
        (recur (z/next z) acc (inc level))
        (recur (z/next z) (cons (+ (count (z/node z)) (* 3 level)) acc) level))
      (apply max (cons (+ (count (z/node z)) (* 3 level)) acc)))))

(defn- max-length
  [z]
  (->> (map #(+ (count (z/node %)) (* 3 (count (z/path %))))
            (filter (complement z/branch?)
                    (take-while (complement z/end?)
                                (iterate z/next z))))
       (apply max)))

(defn- count-path
  [z]
  (if (z/branch? z)
    (* 3 (count (z/path z)))
    (* 3 (- (count (z/path z)) 1))))

(defn- is-left?
  [z]
  (z/right z))

(defn- is-next-branch?
  [z]
  (if-let [r (z/right z)]
    (z/branch? r)))

(defn- update-actives
  [a & hs]
  (swap! a #(reduce (fn [z y]
                      (if (z (+ y 3))
                        (dissoc z (+ y 3))
                        (assoc z (+ y 3) 1)))
                    %
                    hs)))

(defn- levels
  [a h]
  (->> (filter #(> % h) (keys @a))
       sort
       (reduce (fn [x y]
                 (let [a (reduce #(+ %1 (count %2)) 0 x)]
                   (conj x (str (apply str (repeat (- (- y (+ h a)) 1) " ")) "|"))))
               [])
       (apply str)))

(defn- two-nodes?
  [z]
  (and (z/branch? z)
       (z/left z)
       (-> (z/left z) z/branch?)))

(defn- leaf-print
  [z actives h]
  (let [bsh (- h (count-path z))
        ns (str (z/node z)
                " "
                (apply str (repeat (- (- bsh 2) (count (z/node z))) "-"))
                "+"
                (levels actives bsh))
        nl (str (apply str (repeat (- bsh 1) " ")) "|--+" (levels actives (+ bsh 3)))]
    (cond (and (is-left? z)
               (not (is-next-branch? z)))
          (do (update-actives actives bsh) [ns nl])
          (and (is-left? z) (is-next-branch? z))
          (do (update-actives actives bsh (- bsh 3)) [ns nl])
          (and (not (is-left? z)) (z/branch? (z/left z)))
          (do (update-actives actives bsh)
              (update-actives actives (- bsh 3))
              [nl (str (str (z/node z)
                            " "
                            (apply str (repeat (- (- bsh 2) (count (z/node z))) "-"))
                            "+"
                            (levels actives bsh)))])
          :else [ns])))

(defn- branch-print
  [z actives h]
  (let [bbh (- h (count-path z))]
    (when (z/left z)
      (when (two-nodes? z)
        (if-not (> (+ 6 bbh) h) (update-actives actives (+ bbh 3)))
        [(str (apply str (repeat bbh " ")) "  |--+" (levels actives (+ bbh 6)))]))))

(defn dendrogram
  "Returns a collection of strings representing a dendrogram of a
  binary tree."
  [coll]
  (let [z (z/vector-zip coll)
        h (max-length z)
        actives (atom {})]
    (mapcat #(if (z/branch? %)
               (branch-print % actives h)
               (leaf-print % actives h))
            (take-while (complement z/end?)
                        (iterate z/next z)))))

(defn print-dendrogram
  "Prints a dendrogram of a binary tree represented by a collection of
  vectors representing the tree."
  [coll]
  (doseq [l (dendrogram coll)]
    (println l)))

(def ccr [["Total Raisin Bran" ["Just Right Fruit & Nut" ["Product 19" ["Total Whole Grain" ["Just Right Crunchy  Nuggets" "Total Corn Flakes"]]]]] ["Quaker Oatmeal" [["All-Bran with Extra Fiber" ["100% Bran" "All-Bran"]] ["100% Natural Bran" [["Cheerios" "Special K"] [["Puffed Rice" "Puffed Wheat"] [[["Mueslix Crispy Blend" ["Muesli Raisins & Almonds" "Muesli Peaches & Pecans"]] [["Frosted Mini-Wheats" ["Raisin Squares" "Strawberry Fruit Wheats"]] ["Maypo" ["Bran Flakes" [["Fruit & Fibre" ["Fruitful Bran" ["Post Nat. Raisin Bran" "Raisin Bran"]]] ["Great Grains Pecan" ["Honey Nut Cheerios" ["Crispy Wheat & Raisins" ["Cracklin' Oat Bran" [["Oatmeal Raisin Crisp" ["Basic 4" "Nutri-Grain Almond-Raisin"]] [["Double Chex" [["Kix" "Triples"] ["Rice Chex" ["Crispix" ["Corn Flakes" ["Corn Chex" "Rice Krispies"]]]]]] ["Golden Grahams" ["Almond Delight" [["Apple Cinnamon Cheerios" ["Cinnamon Toast Crunch" ["Cap'n Crunch" "Honey Graham Ohs"]]] [["Grape-Nuts" "Nutri-grain Wheat"] [[["Quaker Oat Squares" ["Grape Nuts Flakes" ["Wheat Chex" "Wheaties"]]] ["Life" ["Clusters" "Raisin Nut Bran"]]] ["Bran Chex" ["Golden Crisp" [["Frosted Flakes" "Honey-comb"] ["Corn Pops" [["Multi-Grain Cheerios" ["Nut&Honey Crunch" "Wheaties Honey Gold"]] ["Apple Jacks" [["Lucky Charms" [["Fruity Pebbles" "Trix"] ["Cocoa Puffs" "Count Chocula"]]] ["Froot Loops" "Smacks"]]]]]]]]]]]]]]]]]]]]]]]] ["Cream of Wheat [Quick]" ["Shredded Wheat" ["Shredded Wheat 'n Bran" "Shredded Wheat spoon size"]]]]]]]]]])
