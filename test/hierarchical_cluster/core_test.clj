(ns hierarchical-cluster.core-test
  (:require [clojure.test :refer :all]
            [clj-classify.core :as cl]
            [clj-recommendation.core :as rec]
            [clojure.java.io :as io]
            [hierarchical-cluster.core :refer :all]
            [clojure.edn :as edn]))

(defonce dog-data [["Border Collie" [20 45] []]
                   ["Boston Terrier" [16 20] []]
                   ["Brittany Spaniel" [18 35] []]
                   ["Bullmastiff" [27 120] []]
                   ["Chihuahua" [8 8] []]
                   ["German Shepherd" [25 78] []]
                   ["Golden Retriever" [23 70] []]
                   ["Great Dane" [32 160] []]
                   ["Portuguese Water Dog" [21 50] []]
                   ["Standard Poodle" [19 65] []]
                   ["Yorkshire Terrier" [6 7] []]])

(defonce dog-cluster [["Yorkshire Terrier" "Chihuahua"]
                      ["Great Dane"
                       ["Bullmastiff"
                        [["German Shepherd" "Golden Retriever"]
                         ["Standard Poodle"
                          ["Boston Terrier"
                           [["Portuguese Water Dog" "Border Collie"] "Brittany Spaniel"]]]]]]])

(defonce dog-dendrogram '("Yorkshire Terrier ----------------------+" "                                        |--+" "Chihuahua ------------------------------+  |" "                                           |--+" "Great Dane -----------------------------+  |" "                                        |--+" "Bullmastiff -------------------------+  |" "                                     |--+" "German Shepherd ---------------+     |" "                               |--+  |" "Golden Retriever --------------+  |  |" "                                  |--+" "Standard Poodle ---------------+  |" "                               |--+" "Boston Terrier -------------+  |" "                            |--+" "Portuguese Water Dog -+     |" "                      |--+  |" "Border Collie --------+  |  |" "                         |--+" "Brittany Spaniel --------+"))

(defonce test-data
  [["A" [1 1]]
   ["B" [1.5 1.5]]
   ["C" [5 5]]
   ["D" [3 4]]
   ["E" [4 4]]
   ["F" [3 3.5]]])

(def cars (edn/read-string (slurp (io/resource "r-mtcar.clj"))))

(def cereals (edn/read-string (slurp (io/resource "cereal.clj"))))

(defn clj->R
  [data file]
  (with-open [w (io/writer file)]
    (doseq [l data]
      (.write w (->> (take 2 l) flatten (interpose ",") (apply str)))
      (.write w "\n"))))

(deftest cluster-test
  (testing "Clustering"
    (is (= (hierarchical-cluster dog-data) dog-cluster))))

(deftest dendrogram-test
  (testing "Dendrogram"
    (is (= (-> (hierarchical-cluster dog-data) dendrogram)
           dog-dendrogram))))

