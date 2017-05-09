(ns hierarchical-cluster.core-test
  (:require [clojure.test :refer :all]
            [hierarchical-cluster.core :refer :all]))

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

(defonce dog-dendrogram '("Yorkshire Terrier ----------------------------+" "                                              |--+" "Chihuahua ------------------------------------+  |" "                                                 |--+" "Great Dane -----------------------------------+  |" "                                              |--+" "Bullmastiff -------------------------------+  |" "                                           |--+" "German Shepherd ---------------------+     |" "                                     |--+  |" "Golden Retriever --------------------+  |  |" "                                        |--+" "Standard Poodle ---------------------+  |" "                                     |--+" "Boston Terrier -------------------+  |" "                                  |--+" "Portuguese Water Dog -------+     |" "                            |--+  |" "Border Collie --------------+  |  |" "                               |--+" "Brittany Spaniel --------------+"))

(deftest cluster-test
  (testing "Clustering"
    (is (= (-> (hierarchical-cluster dog-data) first) dog-cluster))))

(deftest dendrogram-test
  (testing "Dendrogram"
    (is (= (-> (hierarchical-cluster dog-data) first dendrogram)
           dog-dendrogram))))

