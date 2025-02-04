import qualified Data.List
import qualified Data.Array
import qualified Data.Bits

-- PFL 2024/2025 Practical assignment 1

-- Uncomment the some/all of the first three lines to import the modules, do not change the code of these lines.

type City = String
type Path = [City]
type Distance = Int

type RoadMap = [(City,City,Distance)]

type AdjList = [(City,[(City,Distance)])]
type AdjMatrix = Data.Array.Array (Int,Int) (Distance)
data AdjPointers = Place City [(AdjPointers, Distance)]

------------------------------------------------------------------------------
--auxiliary function that inserts the cities into the cities list but verifies first if they were already in it
citiesAux :: (City, City, Distance) -> [City] -> [City]
citiesAux (city1, city2, _) list
    | city1 `notElem` list && city2 `notElem` list = city1 : city2 : list
    | city1 `notElem` list = city1 : list
    | city2 `notElem` list = city2 : list
    | otherwise = list

--Prints all the cities present in the graph (uses a auxiliary function to insert the  cities)
cities :: RoadMap -> [City]
cities = foldr citiesAux []

------------------------------------------------------------------------------
--Checks if two cities are directly connected
areAdjacent :: RoadMap -> City -> City -> Bool
areAdjacent [] _ _ = False -- no connection found
areAdjacent graph city1 city2
    | city1 == c1 && city2 == c2 = True
    | city1 == c2 && city2 == c1 = True
    | otherwise = areAdjacent (tail graph) city1 city2
    where (c1,c2,_) = head graph

-------------------------------------------------------------------------------
--Finds the distance between two cities that are conncted directly. Just a distance if they are connected or Nothing otherwise
distance :: RoadMap -> City -> City -> Maybe Distance
distance graph city1 city2
    | null graph = Nothing
    | city1 == c1 && city2 == c2 = Just dist
    | city2 == c1 && city1 == c2 = Just dist
    | otherwise = distance (tail graph) city1 city2
    where (c1,c2,dist) = head graph

-------------------------------------------------------------------------------
--finds all the adjacent cities to a particular city
adjacent :: RoadMap -> City -> [(City,Distance)]
adjacent [] _ = []
adjacent graph city
    | city == c1 = (c2,dist) : adjacent (tail graph) city
    | city == c2 = (c1,dist) : adjacent (tail graph) city
    | otherwise = adjacent (tail graph) city
    where (c1, c2, dist) = head graph

------------------------------------------------------------------------------

--auxiliary function that accumulates the distance on the path
pathDistanceAux :: Maybe Distance -> Maybe Distance -> Maybe Distance
pathDistanceAux Nothing acc = Nothing
pathDistanceAux dist Nothing = dist
pathDistanceAux (Just dist)  (Just acc) = Just (acc + dist)

--Returns the sum of all individual distances in a path between two cities. Just a value is there are connections or Nothing otherwise
pathDistance :: RoadMap -> Path -> Maybe Distance
pathDistance [] _ = Nothing
pathDistance _ [] = Nothing
pathDistance graph path
    | length path == 1 = Just 0
    | dist /= Nothing = pathDistanceAux (pathDistance graph (tail path)) dist
    | otherwise = Nothing
    where dist = distance graph (head path) (head (tail path))

------------------------------------------------------------------------------

--Auxiliary function that pairs each city to the number of it´s adjacents
cityAdjacent :: RoadMap -> [City] -> [(City,Int)]
cityAdjacent _ [] = []
cityAdjacent graph listOfCities =  (head listOfCities , length (adjacent graph (head listOfCities))) : cityAdjacent graph (tail listOfCities)

--Auxiliary function that returns the cities with most degree
romeAux :: [(City,Int)] -> Int -> [City]
romeAux [] _ = []
romeAux cityDegree max
    | max == degree = cityname : romeAux (tail cityDegree) max
    | otherwise =  romeAux (tail cityDegree) max
    where (cityname,degree) = head cityDegree

-- Returns the names of the cities with the highest number of roads connecting to them. It uses maximum to find the highest degree and then gets the cities with that degree
rome :: RoadMap -> [City]
rome [] = []
rome graph = romeAux (cityAdjacent graph (cities graph)) (maximum (map snd (cityAdjacent graph (cities graph))))

------------------------------------------------------------------------------
--Auxiliary function that checks if a city is connected to all of the other cities. It verifies if it as the same number of adjacent cities as the total number of cities in the graph - 1 (i.e, minus itself)
isConnectedToAll :: RoadMap -> City -> Bool
isConnectedToAll graph city = length (adjacent graph city) == length (cities graph) - 1

--Auxiliary function that transverses the list of cities present in the graph and checks if every city is reachable from the others
isStronglyConnectedAux :: RoadMap -> [City] -> Bool
isStronglyConnectedAux _ [] = True --checked every city and they are all reachable from the other cities
isStronglyConnectedAux graph cityList
    | isConnectedToAll graph city  = isStronglyConnectedAux graph (tail cityList)
    | otherwise = False
    where city = head cityList


--Checks if a graph is strongly connected. p (i.e., if every city is reachable from every other city).
isStronglyConnected :: RoadMap -> Bool
isStronglyConnected graph = isStronglyConnectedAux graph (cities graph)
-------------------------------------------------------------------------------------------------------

--Calculates the distance between two cities but returns Distance instead of Maybe Distance
distanceInt :: RoadMap -> City -> City -> Distance
distanceInt graph city1 city2
    | null graph = 0
    | city1 == c1 && city2 == c2 = dist
    | city2 == c1 && city1 == c2 = dist
    | otherwise = distanceInt (tail graph) city1 city2
    where (c1,c2,dist) = head graph

--Accumulator to calculate the distance in a path but returns Distance instead of Maybe Distance
pathDistanceIntAux :: Distance -> Distance -> Distance
pathDistanceIntAux 0 acc = acc
pathDistanceIntAux dist 0 = dist
pathDistanceIntAux dist acc = acc + dist

--Calculates the distance in a path but returns Distance instead of Maybe Distance
pathDistanceInt :: RoadMap -> Path -> Distance
pathDistanceInt [] _ = 0
pathDistanceInt _ [] = 0
pathDistanceInt graph path
    | length path == 1 = 0
    | dist /= 0 = pathDistanceIntAux (pathDistanceInt graph (tail path)) dist
    | otherwise = 0
    where dist = distanceInt graph (head path) (head (tail path))


-- Auxiliary function for shortestPath
-- Iterates through the start city and it's neighbours, keeps the current path distance and a list of already visited cities
-- Prunes the path if it costs already more than the minCost path in the result, otherwhise clears the result and puts the new minCost path
-- At the end returns the result with only the paths with minCost in the format of (Distance, [Path])
shortestPathAux :: RoadMap -> City -> City -> [City] -> Path -> Distance -> (Distance, [Path]) -> (Distance, [Path])
shortestPathAux graph current dest visited path currentCost (minCost, paths)
    | current == dest =
        let newCost = pathDistanceInt graph (reverse (current : path))
        in if newCost < minCost
           then (newCost, [reverse (current : path)])  -- New best path with a lower cost
           else if newCost == minCost
                then (minCost, reverse (current : path) : paths)  -- Path with the same minimum cost
                else (minCost, paths)  -- Discard this path as it is more costly
    | otherwise = foldl explore (minCost, paths) neighbors
  where
    neighbors = adjacent graph current
    explore (bestCost, bestPaths) (neighbor, edgeCost)
        | neighbor `elem` visited = (bestCost, bestPaths)  -- Skip already visited nodes
        | currentCost + edgeCost > bestCost = (bestCost, bestPaths)  -- Prune if the path exceeds current min cost
        | otherwise = shortestPathAux graph neighbor dest (neighbor : visited) (current : path) (currentCost + edgeCost) (bestCost, bestPaths)

-- Main shortestPath function
-- Calls shortestPathAux for the recursive iteration
-- Only returns the paths from shortestPathAux [Path]
shortestPath :: RoadMap -> City -> City -> [Path]
shortestPath _ [] [] = []
shortestPath _ [] c2 = [[c2]]
shortestPath _ c1 [] = [[c1]]
shortestPath graph c1 c2 =
        let (_, paths) = shortestPathAux graph c1 c2 [c1] [] 0 (maxBound, [])  -- Initialize minCost to maxBound
        in paths

------------------------------------------------------------------------------

--data types specifically designed for the tsp problem
type Mask = Int
type AdjMatrixMask = Data.Array.Array (Int,Int) (Distance, Path) --only differs from the adjMatrix because it adds the path found 

--Converts a city to a int
cityToInt :: City -> Int
cityToInt city = read city :: Int

--Converts a int into a city
intToCity :: Int -> City
intToCity = show

--Returns the maximum value a mask can have
allVisited :: Int -> Mask
allVisited len =  (1 `Data.Bits.shiftL` len) - 1 :: Int

--Check if a city is already visited using a bitwise operation
isVisited :: City -> Mask -> Bool
isVisited city mask = mask Data.Bits..&. (1 `Data.Bits.shiftL` cityToInt city) /= 0

--updates a mask by tagging a city as visited
updateMask :: City -> Mask -> Mask
updateMask city mask = mask Data.Bits..|. (1 `Data.Bits.shiftL` cityToInt city)

-- checks if the connection between two cities exists
hasConnection :: AdjMatrix -> City -> City -> Bool
hasConnection distMatrix city1 city2 =  distMatrix Data.Array.! (read city1, read city2 ) /= -1

--checks if we already founded a path from a specific city and visited cities (stored in the masks matrix)
pathAlreadyFound :: AdjMatrixMask -> Mask -> City -> Bool
pathAlreadyFound maskMatrix mask city = dist /= (-1)
    where (dist,path) = maskMatrix Data.Array.! (read city, mask)

--updates the mask matrix if the distance found is less than the one already in the matrix
updateMaskMatrix :: AdjMatrixMask -> Mask -> City -> Distance -> Path -> AdjMatrixMask
updateMaskMatrix maskMatrix mask city newDist newPath
    | not (pathAlreadyFound maskMatrix mask city) = updatedMatrix   --There is no path found so we can insert a new one
    | not (pathAlreadyFound maskMatrix mask city) && (newDist < dist) = updatedMatrix -- there is already a path and the new distance was less than the old distance
    | otherwise = maskMatrix --No changes needed
    where
        updatedMatrix = maskMatrix Data.Array.// [((read city, mask), (newDist,newPath))]
        (dist,path) = maskMatrix Data.Array.! (read city, mask)

--updates the current distance by adding the distance between two visited cities
updateDistance :: AdjMatrix -> City -> City-> Distance  -> Distance
updateDistance distMatrix city1 city2 currentDist = currentDist + distMatrix Data.Array.! (cityToInt city1, cityToInt city2)

--fills the matrix with the distances between the cities
fillDistMatrix :: RoadMap -> AdjMatrix -> AdjMatrix
fillDistMatrix [] matrix = matrix  --inserted all the distances
fillDistMatrix graph matrix = fillDistMatrix (tail graph) newMatrix
    where
        (city1,city2,dist) = head graph
        newMatrix = matrix Data.Array.// [((cityToInt city1, cityToInt city2 ), dist), ((cityToInt city2 , cityToInt  city1), dist)]

--creates the matrix that will store the distance between the cities. If two cities are not connected, the distance will be -1 (invalid)
createDistMatrix :: RoadMap -> Int -> AdjMatrix
createDistMatrix graph len = fillDistMatrix graph (Data.Array.array ((0, 0), (len-1, len-1)) [((i,j), -1) | i <- [0..(len-1)], j <- [0..(len-1)] ])

--creates the matrix that will store the minimum distance to the inicial city from a given city and path 
createMaskMatrix :: RoadMap -> Int -> AdjMatrixMask
createMaskMatrix graph len = Data.Array.array ((0, 0), (len - 1, maxMask)) [((i,j), (-1, [])) | i <- [0..(len-1)], j <- [0..maxMask] ]
    where maxMask = (1 `Data.Bits.shiftL` len) - 1 :: Int  -- maximum value a mask will have

-- Finds the path with the minimum distance in a list of paths and accumulated distances
findMinPath :: [(AdjMatrixMask, Distance, Path)] -> (AdjMatrixMask, Distance, Path) -> (AdjMatrixMask, Distance, Path)
findMinPath [] minTuple -- base case
 | minDist == (maxBound :: Int) = (minMatrix, -1, []) -- didn't found a feasable minimum path
 | otherwise = minTuple
 where (minMatrix, minDist, minPath) = minTuple
findMinPath pathList minTuple
    | (dist < minDist) && (dist /= (-1)) = findMinPath (tail pathList) (mMatrix, dist, path)
    | otherwise =  findMinPath (tail pathList) (minMatrix, minDist, minPath)
    where
        (mMatrix, dist, path) = head pathList
        (minMatrix, minDist, minPath) = minTuple

-- Auxiliary function to the travelSalesRec used to iterate throught the cities and verifies which weren't visited and have connection to the city we are visiting. 
-- Calls again travelSalesRec for the cities that weren't visited. 
-- Stores a list with the paths found as an auxiliary value
-- Return the path in the format (AdjMatrix, Distance, Path) where distance is minimum and is diferent from -1.
travelSalesRecIteration :: City -> City -> AdjMatrix -> AdjMatrixMask -> Distance -> Mask -> Int -> [(AdjMatrixMask, Distance, Path)] -> (AdjMatrixMask, Distance, Path)
travelSalesRecIteration originCity newCity distMatrix maskMatrix currentDist mask numbCities listPaths
    | cityToInt newCity == numbCities = findMinPath listPaths (maskMatrix, maxBound :: Int, [])                 -- iterated througth all the cities. Return the minimum path found in the iteration
    | not (isVisited newCity mask) && hasConnection distMatrix originCity newCity = nextIterationWithPath       -- found a unvisited city that has a path to the current city. Continue the recursion
    | otherwise = nextIterationNoPath                                                                           -- the city has already visited or didn't have a a path to the current city. Continue the search for an unvisited city
    where
        updatedDistance = updateDistance distMatrix originCity newCity currentDist
        updatedMask =  updateMask newCity mask
        (mMatrix, dist, path) = travelSalesRec newCity distMatrix maskMatrix updatedDistance updatedMask numbCities
        updatedMaskMatrix = updateMaskMatrix maskMatrix mask newCity dist path
        newListPaths = listPaths ++ [(mMatrix, dist, newCity : path)]
        nextIterationNoPath = travelSalesRecIteration originCity (intToCity (cityToInt newCity + 1)) distMatrix updatedMaskMatrix currentDist mask numbCities listPaths
        nextIterationWithPath = travelSalesRecIteration originCity (intToCity (cityToInt newCity + 1)) distMatrix updatedMaskMatrix  currentDist mask numbCities newListPaths



--Recursive part of the problem. 
--Receives the current city we are visiting, the auxiliary matrices (distance and mask), the visited cities mask, number of cities.
--Returns a tuple with the updated mask matrix, the distance found and the path found.
travelSalesRec :: City -> AdjMatrix -> AdjMatrixMask -> Distance -> Mask -> Int -> (AdjMatrixMask, Distance, Path)
travelSalesRec city distMatrix maskMatrix currentDist mask numbCities
    | mask == allVisited numbCities && hasConnection distMatrix city "0" = returnTuple                      -- found a path
    | pathAlreadyFound maskMatrix mask city = (maskMatrix, distFound, pathFound)                            -- path already previously
    | otherwise = travelSalesRecIteration city "0" distMatrix maskMatrix currentDist mask numbCities []     -- No Path found. Continue the search
    where
        returnTuple = (maskMatrix, currentDist + distMatrix Data.Array.! (cityToInt city , 0), [])
        (distFound, pathFound) = maskMatrix Data.Array.! (read city, mask)


-- Organizes the data that will be sent to the recursive part and returns the path found by the recursive part
travelSalesDataOrganizer :: RoadMap -> City -> Int -> Path
travelSalesDataOrganizer graph originCity numbCities
    | path /= [] = [originCity] ++ path ++ [originCity] -- found path so we add the origin/finish city
    | otherwise = path
    where (matrix, dist, path) = travelSalesRec originCity (createDistMatrix graph numbCities) (createMaskMatrix graph numbCities) 0 1 numbCities

-- Executes the TSP algorithm with the starting city being the city "0"
travelSales :: RoadMap -> Path
travelSales graph = travelSalesDataOrganizer graph "0" (length (cities graph))

-----------------------------------------------------------------------------------------------------------------------------

tspBruteForce :: RoadMap -> Path
tspBruteForce = undefined -- only for groups of 3 people; groups of 2 people: do not edit this function

-----------------------------------------------------------------------------------------------------------------------------
-- Some graphs to test your work
gTest1 :: RoadMap
gTest1 = [("7","6",1),("8","2",2),("6","5",2),("0","1",4),("2","5",4),("8","6",6),("2","3",7),("7","8",7),("0","7",8),("1","2",8),("3","4",9),("5","4",10),("1","7",11),("3","5",14)]

gTest2 :: RoadMap
gTest2 = [("0","1",10),("0","2",15),("0","3",20),("1","2",35),("1","3",25),("2","3",30)]

gTest3 :: RoadMap -- unconnected graph
gTest3 = [("0","1",4),("2","3",2)]

gTest4 :: RoadMap
gTest4 = [("0","1",3),("2","3",3), ("0","2",1), ("0", "3", 1), ("1", "2", 1), ("1", "3", 1)]

gTest6 :: RoadMap
gTest6 = [("0","1",1),("2","3",3), ("0","2",1), ("0", "3", 3), ("1", "2", 3), ("1", "3", 1), ("4", "3", 1), ("4", "2", 1)]

gTest5 :: RoadMap
gTest5 = [
    ("0","1",1),
    ("1","2",1),
    ("0","2",2),    -- Direct path from 0 to 2
    ("2","3",1),
    ("1","3",2)]