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

## Travel Sales Implementation


Our implementation of the Traveling Salesman Problem (TSP) employs a dynamic programming approach. This method utilizes data structures like adjacency matrices to enhance performance and prevent redundant computations


In our implementation, we decided to use two data structures with similar characteristics:
- **AdjMatrix: Data.Array.Array (Int, Int) Distance**, used to store the graph distances. This allows us to quickly access the distances in (O(1)) time, without needing to traverse the graph’s Roadmap representation, which would take (O(n)) time.
- **AdjMatrixMask: Data.Array.Array (Int, Int) (Distance, Path)**, used to associate a visited cities mask with the currently visited city. This helps us avoid unnecessary computations by storing the minimum distance path for a specific set of visited cities (mask) and the currently visited city. When we visit the same cities and reach the associated city in future computations, we don’t need to recalculate the minimum path and distance, as it is already stored in the matrix.


To reduce the complexity of checking visited cities, we use a bit mask with a size equal to the number of cities. To check if a city is visited, we simply verify if the bit at the city’s position is set to 1. These bit manipulation operations are much faster and more efficient than a simple search in a list.

**Algorithm Description:**

We start our algorithm by creating the two previously mentioned data structures. We populate the distance matrix with the distances stored in the Roadmap and initialize the mask matrix with default values (-1 for the distance and [] for the path). These two data structures are created by the function ``travelSalesDataOrganizer``, which takes the origin city, the graph, and the number of cities in the graph as inputs.


After this, the ``travelSalesDataOrganizer`` calls the ``travelSalesRec`` function, which performs the recursive part of the algorithm. This function first checks if all cities have been visited and if the currently visited city has a connection to the origin. If both conditions are met, it returns a tuple with the updated mask matrix, the distance, and the path found.
If the first condition is not met, it then checks if a minimum path from the current city with the already visited cities has been previously found. This is done by verifying in the mask matrix if there is a valid path for the currently visited city and the mask of the visited cities. If this condition is true, the function returns the previously found path and distance.
If none of the previous conditions are met, the ``travelSalesRec`` function calls the ``travelSalesRecIteration`` function.


The ``travelSalesRecIteration`` function checks which cities haven’t been visited and attempts to find a path using an unvisited city. To do this, it calls the ``travelSalesRec`` function again to continue the recursion. If the unvisited city doesn’t have a connection to the currently visited city, the search for an unvisited city continues. Once all cities have been checked, this function selects the path with the minimum distance from the paths found and returns it.


At the end, after completing the recursive part of the problem, the ``travelSalesRec`` function will return the minimum path to the ``travelSalesDataOrganizer``. This function will then organize the data to be received by the ``travelSales`` function, which is the main/original function.







