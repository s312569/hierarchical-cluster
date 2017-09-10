(ns hierarchical-cluster.core
  (:require [clj-recommendation.core :as rec]
            [clj-classify.core :as cl]
            [clojure.data.priority-map :as pm]
            [clojure.zip :as z]
            [clojure.edn :as edn]))

(declare update-similarities-new)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; utilities
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- init-queue-new
  [c]
  (let [k (cl/knn-classifier (:data c) :norm (:norm c) :dist-method (:dist c) :k (count (:data c)))
        pm (pm/priority-map-keyfn-by (queue-key c) (queue-comp c))]
    (->> (map (fn [[cl _ _ :as x]]
                (let [p (pm/priority-map-keyfn-by first (sim-sort-order c))
                      nns (->> (rec/nearest-neighbours k x)
                               (filter #((complement #{cl}) (first %)))
                               (map (fn [[k v]] [[cl k] [(- (/ 1 v) 1) 1]]))
                               (into p))
                      nn (first nns)]
                  [(-> (first nns) second) cl cl nns]))
              (:data c))
         (map (fn [c x] [c x]) (iterate inc 0))
         (into pm))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; records
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defrecord centroidClusterer [data norm dist]
  
  Cluster
  
  (queue-key [c]
    (fn [[xsc _ _ xsim :as x]]
      (-> (cons xsc (-> xsim first first sort)) vec)))

  (queue-comp [c]
    (fn [[xsc & xkeys] [ysc & ykeys]]
      (compare (-> (cons xsc ykeys) vec) (-> (cons ysc xkeys) vec))))

  (sim-sort-order [c] <)

  (cluster [c]
    (loop [q (init-queue-new c)]
      (let [[[xind [xsc xcl xkey xsim] :as x] [yind [ysc ycl ykey ysim] :as y] & _] (seq q)]
        (if yind
          (let [new-cl (with-meta [xcl ycl] {:distance xsc})
                cl-count (count (flatten new-cl))
                new-sims (merge xsim ysim)
                new-i (pmap (partial (distance-updater c [xind xkey ykey cl-count new-sims]))
                            (-> (pop q) pop))]
            (recur (conj (->> (map first new-i) (into (empty q)))
                         (let [cl-sims (->> (map second new-i) (into (empty xsim)))]
                           [xind [(-> (peek cl-sims) second first) new-cl xkey cl-sims]]))))
          xcl))))

  (distance-updater [c [x-ind x-key y-key cl-count sims]]
    (fn [[entry-index [esc entry-cl entry-key entry-sims]]]
      (let [[hgd hco] (get sims [x-key entry-key])
            [igd ico] (get sims [y-key entry-key])
            [hid _] (get sims [x-key y-key])
            new-dist (+ (* (/ hco (+ hco ico)) hgd)
                        (* (/ ico (+ hco ico)) igd)
                        (* (/ (* hco ico) (* (+ hco ico) (+ hco ico))) (- 1 hid)))
            eco (if (vector? entry-cl) (-> (flatten entry-cl) count) 1)
            new-e-sims (assoc (reduce (fn [x y] (dissoc x [entry-key y])) entry-sims [x-key y-key])
                              [entry-key x-key] [new-dist eco])]
        [[entry-index [(-> (peek new-e-sims) second first) entry-cl entry-key new-e-sims]]
         [[x-key entry-key] [new-dist cl-count]]]))))

(defrecord completeClusterer [data norm dist]

  Cluster

  (queue-key [c]
    (fn [[xsc _ _ xsim :as x]]
      (-> (cons xsc (-> xsim first first sort)) vec)))

  (queue-comp [c]
    (fn [[xsc & xkeys] [ysc & ykeys]]
      (compare (-> (cons ysc xkeys) vec) (-> (cons xsc ykeys) vec))))

  (sim-sort-order [c] >)

  (cluster [c]
    (loop [q (init-queue-new c)]
      (let [[[xind [xsc xcl xkey xsim]] [yind [ysc ycl ykey ysim]] & _] (seq q)]
        (if yind
          (let [new-cl (with-meta [xcl ycl] {:distance xsc})
                new-sims (merge xsim ysim)
                sim-set (-> (flatten new-cl) set)
                new-i (pmap (partial (distance-updater c [new-sims sim-set])) (-> (pop q) pop))]
            (recur (conj (->> (map first new-i) (into (empty q)))
                         (let [cl-sims (->> (map second new-i) (into (empty xsim)))]
                           [xind [(-> (peek cl-sims) second first) new-cl xkey cl-sims]]))))
          xcl))))

  (distance-updater [c [sims sim-set]]
    (fn [[entry-index [esc entry-cl entry-key entry-sims]]]
      (let [e-cl-set (if (vector? entry-cl) (-> (flatten entry-cl) set) #{entry-cl})
            [nk nd] (-> (drop-while (fn [[[x-key e-key] v]]
                                      (not (e-cl-set e-key)))
                                    (rseq sims))
                        first)
            new-e-sims (assoc (reduce (fn [x y] (dissoc x [entry-key y])) entry-sims sim-set)
                              nk nd)]
        [[entry-index [(-> (peek new-e-sims) second first) entry-cl entry-key entry-sims]]
         [nk nd]]))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; cluster
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- init-queue
  [k t]
  (let [qk (fn [[xsc _ xsim _]]
             (-> (cons (if (number? xsc) xsc (first xsc))
                       (-> xsim first first sort))
                 vec))
        qc (fn [x y]
             (compare (-> (cons (first y) (drop 1 x)) vec)
                      (-> (cons (first x) (drop 1 y)) vec)))
        pm (pm/priority-map-keyfn-by qk qc)]
    (->> (map (fn [x]
                (let [p (pm/priority-map-keyfn-by (fn [x] (if (number? x) x (first x)))
                                                  (if (= t :centroid) < >))
                      nns (->> (rec/nearest-neighbours k x)
                               (filter #((complement #{(first x)}) (first %)))
                               (map (fn [[k v]]
                                      [[(first x) k]
                                       (if (= t :centroid)
                                         (- (/ 1 v) 1)
                                         v)]))
                               (into p))
                      nn (first nns)]
                  [(second nn) (first x) nns]))
              (:data k))
         (map (fn [c x] [c x]) (iterate inc 0))
         (into pm))))

(defmulti cluster-distance (fn [args t] t))

(defmethod cluster-distance :complete
  [[e-cl-set _ sims] _]
  (let [r (->> (drop-while (fn [[k v]]
                             (not (e-cl-set (second k))))
                           (rseq sims))
               first)]
    [r [(-> (first r) reverse) (second r)]]))

(defmethod cluster-distance :average
  [[e-cl-set sim-set sims] _]
  (let [ks (mapcat (fn [x]
                     (reduce (fn [z y] (conj z [x y]))
                             []
                             e-cl-set))
                   sim-set)
        dists (->> (select-keys sims (vec ks))
                   (map (fn [[k v]] (if (number? v) [k [v 1]] [k v]))))
        total (->> (map second dists) (map #(apply * %)) (reduce +))
        count (->> (map second dists) (map second) (reduce +))
        avg (/ total count)]
    [[(-> (first dists) first) [avg count]]
     [(-> (first dists) first reverse) [avg count]]]))

(defmethod cluster-distance :centroid
  [[e-cl-set sim-set sims] _]
  (let [[[hk [hgd hco]] [ik [igd ico]]]
        (->> (filter (fn [[k v]] (and (sim-set (first k))) (e-cl-set (second k))) sims)
             (map (fn [[k v]] (if (number? v) [k [v 1]] [k v]))))
        hid (->> (filter (fn [[k v]] (every? sim-set k)) sims)
                 (map (fn [[k v]] (if (number? v) v (first v))))
                 first)
        new-dist (+ (* (/ hco (+ hco ico)) hgd)
                    (* (/ ico (+ hco ico)) igd)
                    (* (/ (* hco ico) (* (+ hco ico) (+ hco ico))) (- 1 hid)))
        ;; t (println ico sim-set e-cl-set [[hk [hgd hco]] [ik [igd ico]]] new-dist
        ;;             (filter (fn [[k v]] (every? sim-set k)) sims)
        ;;             "\n")
        ]
    [[[(first sim-set) (first e-cl-set)] [new-dist (count sim-set)]]
     [[(first e-cl-set) (first sim-set)] [new-dist (count e-cl-set)]]]))

(defn- update-similarities
  [[index cluster sims] old-queue t]
  (let [sim-set (-> cluster flatten set)
        nq (->> (map (fn [[entry-index [_ entry-cl entry-sims]]]
                       (let [e-cl-set (if (vector? entry-cl)
                                        (-> entry-cl flatten set)
                                        #{entry-cl})
                             [sgdist egdist] (cluster-distance [e-cl-set sim-set sims] t)
                             new-sims (conj (->> (filter (fn [[k v]] (not (sim-set (second k))))
                                                         entry-sims)
                                                 (into (empty entry-sims)))
                                            egdist)]
                         [[entry-index [(-> (peek new-sims) second) entry-cl new-sims]]
                          sgdist]))
                     old-queue))]
    ;; (println (conj (->> (map first nq) (into (empty old-queue)))
    ;;                (let [n (->> (map second nq) (into (empty sims)))]
    ;;                  [index [(-> (peek n) second) cluster n]])))
    (conj (->> (map first nq) (into (empty old-queue)))
          (let [n (->> (map second nq) (into (empty sims)))]
            [index [(-> (peek n) second) cluster n]]))))

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

(defmulti cluster-old (fn [q t] t))

(defmethod cluster-old :default
  [q t]
  (loop [q q]
    (let [[[xind [xsc xcl xsim]] [yind [ysc ycl ysim]] & _] (seq q)]
      (if yind
        (let [new-cl (with-meta [xcl ycl] {:distance xsc})]
          (recur (update-similarities [xind new-cl (merge xsim ysim)] (-> (pop q) pop) t)))
        xcl))))

(defmethod cluster-old :single
  [q _]
  (loop [q q]
    (let [[[xind [xsc xcl xsim]] [yind [ysc ycl ysim]] & _] (seq q)]
      (if yind
        (let [mn (merge-pms xsim ysim)
              cl (with-meta [ycl xcl] {:distance xsc})]
          (recur (conj (-> (pop q) pop) [xind [(-> (peek mn) second) cl mn]])))
        xcl))))

(defmethod cluster-old :centroid
  [q t]
  (loop [q q]
    (let [[[xind [xsc xcl xsim]] [yind [ysc ycl ysim]] & _] (seq q)]
      (if yind
        (let [new-cl (with-meta [xcl ycl] {:distance xsc})
              new-sims (merge xsim ysim)]
          (recur (update-similarities [xind new-cl new-sims] (-> (pop q) pop) t)))
        xcl))))

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
  (let [k (cl/knn-classifier data :dist-method dist-method :norm norm-method :k (count data))
        q (init-queue k type)]
    (cluster-old q type)))

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

