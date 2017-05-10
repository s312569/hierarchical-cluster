# hierarchical-cluster

A Clojure library for hierachial clustering. Written for educational
purposes and in beta.

## To use

Add the library to your project.clj file:

```clojure
[hierarchical-cluster "0.1.1"]
```

Then require:

```clojure
(:require [hierarchical-cluster :as hc])
```

## Usage

The function `hierarchical-cluster` will return a vector containing a
nested vector of vectors representing a binary tree as the first
element and a hash-map containing distance values as the second
element. Data should be a collection of vectors with the class of an
item as the first element, a vector of numerical attribute values as
the second element and an optional vector of comments as the third
element:

```clojure
user> (def dog-data [["Border Collie" [20 45] []]
                    ["Boston Terrier" [16 20] []] ...])
#'user/dog-data
user> (hierarchical-cluster dog-data :dist-method :euclidean :norm-method :mod-standard-score)
([["Yorkshire Terrier" "Chihuahua"] ["Great Dane" ["Bullmastiff" ...]]]
 {#{"Portuguese Water Dog" "Border Collie"} 0.811879937360228, #{"Chihuahua" "Yorkshire Terrier"}
 0.7343071967365381, ...})
user>
```

To print a dendrogram use the `print-dendrogram` function on the first
element of the vector returned by `hierarchical-cluster`:

```clojure
user> (->> (hierarchical-cluster dog-data) first print-dendrogram)
Yorkshire Terrier ----------------------------+
                                              |--+
Chihuahua ------------------------------------+  |
                                                 |--+
Great Dane -----------------------------------+  |
                                              |--+
Bullmastiff -------------------------------+  |
                                           |--+
German Shepherd ---------------------+     |
                                     |--+  |
Golden Retriever --------------------+  |  |
                                        |--+
Standard Poodle ---------------------+  |
                                     |--+
Boston Terrier -------------------+  |
                                  |--+
Portuguese Water Dog -------+     |
                            |--+  |
Border Collie --------------+  |  |
                               |--+
Brittany Spaniel --------------+
```

## License

Copyright © 2017 Jason Mulvenna

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
