# PFL Project 1

## Group Members

Group 3, Class 4

- Lucas Faria, up202207540
- Pedro Borges, up202207552


## Contributions

 **cities**:
 - Lucas Faria: 95%, did all the implementation and some testing/performance improvements
 - Pedro Borges: 5%, helped with the testing

**areAdjacent**:
- Lucas Faria: 95%, did all the implementation and some testing/performance improvements
- Pedro Borges: 5%, helped with the testing

**distance**: 
- Lucas Faria: 5%, helped with the testing. 
- Pedro Borges: 95%, did all the implementation and some testing/performance improvements.

**adjacent**:
- Lucas Faria: 95%, did all the implementation and some testing/performance improvements.
- Pedro Borges: 5%, helped with the testing.

**pathDistance**:
- Lucas Faria: 5%, helped with the testing. 
- Pedro Borges: 95%, did all the implementation and some testing/performance improvements.

**rome**:
- Lucas Faria: 5%, helped with the testing. 
- Pedro Borges: 95%, did all the implementation and some testing/performance improvements.

**isStronglyConnected**:
- Lucas Faria: 95%, did all the implementation and some testing/performance improvements.
- Pedro Borges: 5%, helped with the testing.

**shortestPath**:
- Lucas Faria: 5%,  helped with the testing.
- Pedro Borges: 95%, did all the implementation and some testing/performance improvements.

**travelSales**:
- Lucas Faria: 95%, did all the implementation and some testing/performance improvements.
- Pedro Borges: 5%, helped with the testing.

## Shortest Path Implementation

Our implementation of the Shortest Path Problem uses a brute-force with pruning approach. This method doesn't utilize any advanced data structures like priority queues, hash tables or graphs represented as adjacency lists or matrices.

**Algorithm Description:**

The algorithm starts by defining a set of core functions that calculate distances between cities and find the minimum-cost paths within a roadmap (RoadMap) graph.

Distance Calculation Between Two Cities: The ``distanceInt`` function calculates the direct distance between two cities in the RoadMap. It iterates through the list of city connections, returning the distance if a match is found between the two specified cities. If no connection exists, the function returns 0, indicating that either there is no connection or it is the end of the search.

Path Distance Calculation with Accumulator: The helper function ``pathDistanceIntAux`` is designed to accumulate distances while iterating through a path of cities. It ensures that distances of 0 (no distance or the starting point) do not affect the accumulated result. This auxiliary function supports recursive distance calculations over a path.

Path Distance Calculation Over an Entire Path: The ``pathDistanceInt`` function uses ``pathDistanceIntAux`` to compute the total distance across a sequence of cities (a path) in the RoadMap. Starting from the head of the path list, it recursively calculates and accumulates the distance between each pair of adjacent cities until the end of the path. If any segment of the path has no connection (``distanceInt`` returns 0), the function terminates the calculation early by returning 0.

Recursive Shortest Path Search with Pruning: The ``shortestPathAux`` function performs a depth-first search from a starting city to a destination city, keeping track of the path, cumulative cost, and a list of visited cities to prevent revisiting. At each step:

If the destination city is reached, it evaluates the path's cost and updates the result based on the minCost so far:
If the path cost is lower than the current minCost, it updates the minCost and clears previous paths, setting this as the new minimum path.
If the path cost matches the current minCost, it appends the new path to the existing results.
If still searching, it explores neighboring cities (adjacent cities in RoadMap) recursively, using explore to check each neighbor.
If a path exceeds minCost, it is pruned to avoid unnecessary computation.
The final result of ``shortestPathAux`` is a tuple of the minimum path cost and a list of all paths with this cost.
Main Shortest Path Function: The ``shortestPath`` function initializes the search by calling ``shortestPathAux`` with starting parameters, including an initial minCost set to maxBound and an empty list of paths. It also includes cases for special conditions, such as if the two cities are directly connected, where it immediately returns the direct path. The primary result returned by ``shortestPath`` is a list of paths that yield the minimum cost between the two specified cities.


## Travel Sales Implementation


Our implementation of the Traveling Salesman Problem (TSP) employs a dynamic programming approach. This method utilizes data structures like adjacency matrices to enhance performance and prevent redundant computations


In our implementation, we decided to use two data structures with similar characteristics:
- **AdjMatrix: Data.Array.Array (Int, Int) Distance**, used to store the graph distances. This allows us to quickly access the distances in (O(1)) time, without needing to traverse the graph’s Roadmap representation, which would take (O(n)) time.
- **AdjMatrixMask: Data.Array.Array (Int, Int) (Distance, Path)**, used to associate a visited cities mask with the currently visited city. This helps us avoid unnecessary computations by storing the minimum distance path for a specific set of visited cities (mask) and the currently visited city. When we visit the same cities and reach the associated city in future computations, we don’t need to recalculate the minimum path and distance, as it is already stored in the matrix.


To reduce the complexity of checking visited cities, we use a bit mask with a size equal to the number of cities. To check if a city is visited, we simply verify if the bit at the city’s position is set to 1. These bit manipulation operations are much faster and more efficient than a simple search in a list.

To simplify the implementation, we choose the city "0" as the origin of the path.

**Algorithm Description:**

We start our algorithm by creating the two previously mentioned data structures. We populate the distance matrix with the distances stored in the Roadmap and initialize the mask matrix with default values (-1 for the distance and [] for the path). These two data structures are created by the function ``travelSalesDataOrganizer``, which takes the origin city, the graph, and the number of cities in the graph as inputs.


After this, the ``travelSalesDataOrganizer`` calls the ``travelSalesRec`` function, which performs the recursive part of the algorithm. This function first checks if all cities have been visited and if the currently visited city has a connection to the origin. If both conditions are met, it returns a tuple with the updated mask matrix, the distance, and the path found.
If the first condition is not met, it then checks if a minimum path from the current city with the already visited cities has been previously found. This is done by verifying in the mask matrix if there is a valid path for the currently visited city and the mask of the visited cities. If this condition is true, the function returns the previously found path and distance.
If none of the previous conditions are met, the ``travelSalesRec`` function calls the ``travelSalesRecIteration`` function.


The ``travelSalesRecIteration`` function checks which cities haven’t been visited and attempts to find a path using an unvisited city. To do this, it calls the ``travelSalesRec`` function again to continue the recursion. If the unvisited city doesn’t have a connection to the currently visited city, the search for an unvisited city continues. Once all cities have been checked, this function selects the path with the minimum distance from the paths found and returns it.


At the end, after completing the recursive part of the problem, the ``travelSalesRec`` function will return the minimum path to the ``travelSalesDataOrganizer``. This function will then organize the data to be received by the ``travelSales`` function, which is the main/original function.







