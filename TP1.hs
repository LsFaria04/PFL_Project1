{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
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
cities [] = []
cities graph = citiesAux (head graph) (cities (tail graph))

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
pathDistanceAux Nothing acc = acc
pathDistanceAux dist Nothing = dist
pathDistanceAux Nothing Nothing = Nothing
pathDistanceAux (Just dist)  (Just acc) = Just (acc + dist)


pathDistance :: RoadMap -> Path -> Maybe Distance
pathDistance [] _ = Nothing
pathDistance _ [] = Nothing
pathDistance graph path
    | length path == 1 = Nothing
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

distanceInt :: RoadMap -> City -> City -> Distance
distanceInt graph city1 city2
    | null graph = 0
    | city1 == c1 && city2 == c2 = dist
    | city2 == c1 && city1 == c2 = dist
    | otherwise = distanceInt (tail graph) city1 city2
    where (c1,c2,dist) = head graph

pathDistanceIntAux :: Distance -> Distance -> Distance
pathDistanceIntAux 0 acc = acc
pathDistanceIntAux dist 0 = dist
pathDistanceIntAux 0 0 = 0
pathDistanceIntAux dist acc = acc + dist


pathDistanceInt :: RoadMap -> Path -> Int
pathDistanceInt [] _ = 0
pathDistanceInt _ [] = 0
pathDistanceInt graph path  
    | length path == 1 = 0
    | dist /= 0 = pathDistanceIntAux (pathDistanceInt graph (tail path)) dist
    | otherwise = 0
    where dist = distanceInt graph (head path) (head (tail path))


getMinCostPaths :: [(Path,Int)] -> Int -> [(Path,Int)]
getMinCostPaths [] _ = []
getMinCostPaths pathsWithCost minCost 
    | minCost == cost = head pathsWithCost : getMinCostPaths (tail pathsWithCost) minCost
    | otherwise = getMinCostPaths (tail pathsWithCost) minCost
    where (path,cost) = head pathsWithCost

calculatePathCost :: RoadMap -> [Path] -> [(Path,Int)]
calculatePathCost _ [] = []
calculatePathCost graph paths = (head paths , pathDistanceInt graph (head paths)) : calculatePathCost graph (tail paths)


shortestPathAux :: RoadMap -> City -> City -> [City] -> Path -> [Path]
shortestPathAux graph current dest visited path
    | current == dest = [reverse (current:path)]  -- Base case: reached destination
    | otherwise = concatMap explore neighbors  -- Explore all valid neighbors
  where
    neighbors = adjacent graph current  -- Get neighbors of the current city
    explore (neighbor, _)
        | neighbor `elem` visited = []  -- Prune if already visited
        | otherwise = shortestPathAux graph neighbor dest (neighbor : visited) (current : path)
        
-- Main shortest path function
shortestPath :: RoadMap -> City -> City -> [Path]
shortestPath _ [] [] = []
shortestPath _ [] c2 = [[c2]]
shortestPath _ c1 [] = [[c1]]
shortestPath graph c1 c2
    | areAdjacent graph c1 c2 = [[c1, c2]]  -- Directly adjacent case
    | null pathsWithCost = []
    | otherwise = map fst (getMinCostPaths pathsWithCost (minimum (map snd pathsWithCost)))
    where pathsWithCost = calculatePathCost graph (shortestPathAux graph c1 c2 [c1] [] )
------------------------------------------------------------------------------

--1. Usar uma matrix para guardar as distancias entre as cities (caso não haja path entre duas cidades usar valor -1)
--3. Usar uma matrix para guardar as distancias e as cidade visitadas dessa distancia, já vistas (usar as mask e).
--2. Considerando que começa por exemplo na cidade 1
--3. Verificar quais cities não foram visitadas, se existir path entre essa city e a city que estamos a visitar que estamos a visitar, adicionamos a city ao path e continuamos à procura.
--4. Caso já tenhamos 
--4. Em cada recursão devemos verificar se a answer que obtivemos é maior ou é menor e retornar a menor.
--5. No fim devemos dar print 

type Mask = Int --new data type that to represent a mask

--Converts a city to a int
cityToInt :: City -> Int
cityToInt city = read city :: Int 

--Converts a int into a city
intToCity :: Int -> City
intToCity city = show city

--Returns the maximum value a mask can have
allVisited :: Int -> Mask
allVisited len =  (1 `Data.Bits.shiftL` len) - 1 :: Int

--Check if a city is already visited using a bitwise operation
isVisited :: City -> Mask -> Bool
isVisited city mask = mask Data.Bits..&. (1 `Data.Bits.shiftL` (cityToInt city)) /= 0

--updates a mask by tagging a city as visited
updateMask :: City -> Mask -> Mask
updateMask city mask = mask Data.Bits..|. (1 `Data.Bits.shiftL` (cityToInt city))

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
createMaskMatrix :: RoadMap -> Int -> AdjMatrix
createMaskMatrix graph len = Data.Array.array ((0, 0), (len - 1, maxMask)) [((i,j), -1) | i <- [0..(len-1)], j <- [0..maxMask] ]
    where maxMask = (1 `Data.Bits.shiftL` len) - 1 :: Int  -- maximum value a mask will have


--Auxiliary function to the travelSalesRec. Verifies which cities weren't visited. Calls travelSalesRec for the cities that weren't visited. Return the city with the (AdjMatrix, Distance, Path) where distance is minimum and is diferent from -1.
-- Stores a list with the paths found as an auxiliary value
travelSalesRecAux :: City -> City -> AdjMatrix -> AdjMatrix -> Distance -> Distance -> Mask -> Int -> [(AdjMatrix, Distance, Path)] -> (AdjMatrix, Distance, Path)
travelSalesRecAux originCity newCity distMatrix maskMatrix minDist currentDist mask numbCities listPaths
    | cityToInt newCity == numbCities = (maskMatrix, -1, [])    --retorna o minimo da lista de paths ou retorna uma distancia de -1 e uma lista vazia (TODO)
    | isVisited newCity mask == False = travelSalesRecAux originCity (intToCity (cityToInt newCity + 1)) distMatrix maskMatrix minDist currentDist mask numbCities newListPaths   -- found and unvisited city . Add a possible path to the list
    | otherwise = travelSalesRecAux originCity (intToCity (cityToInt newCity + 1)) distMatrix maskMatrix minDist currentDist mask numbCities listPaths
    where newListPaths = listPaths ++ [travelSalesRec newCity distMatrix maskMatrix minDist (updateDistance distMatrix originCity newCity currentDist) (updateMask newCity mask) numbCities] 


--Recursive part of the problem. Receives the current city we are visiting, the auxiliary matrices, the minimum distance found, the visited cities mask, number of cities and return a tuple with the updated mask matrix, the distance found and the path found
travelSalesRec :: City -> AdjMatrix -> AdjMatrix -> Distance -> Distance -> Mask -> Int -> (AdjMatrix, Distance, Path)
travelSalesRec city distMatrix maskMatrix minDist currentDist mask numbCities
    | mask == allVisited numbCities && distMatrix Data.Array.! (cityToInt city, 0) /= (-1) = (maskMatrix, currentDist + distMatrix Data.Array.! (cityToInt city , 0), [city]) --encontramos um caso em que todas as cities foram visitadas e existe ligação da ultima city à origem
    | otherwise = travelSalesRecAux city "0" distMatrix maskMatrix minDist currentDist mask numbCities []

-- Receives the city where we will start the search and return the possible path from that city
travelSalesAux :: RoadMap -> City -> Int -> Path 
travelSalesAux graph originCity numbCities = path
    where (matrix, dist, path) = travelSalesRec originCity (createDistMatrix graph numbCities) (createMaskMatrix graph numbCities) (maxBound :: Int) 0 1 numbCities

travelSales :: RoadMap -> Path
travelSales = undefined

tspBruteForce :: RoadMap -> Path
tspBruteForce = undefined -- only for groups of 3 people; groups of 2 people: do not edit this function

-- Some graphs to test your work
gTest1 :: RoadMap
gTest1 = [("7","6",1),("8","2",2),("6","5",2),("0","1",4),("2","5",4),("8","6",6),("2","3",7),("7","8",7),("0","7",8),("1","2",8),("3","4",9),("5","4",10),("1","7",11),("3","5",14)]

gTest2 :: RoadMap
gTest2 = [("0","1",10),("0","2",15),("0","3",20),("1","2",35),("1","3",25),("2","3",30)]

gTest3 :: RoadMap -- unconnected graph
gTest3 = [("0","1",4),("2","3",2)]

gTest4 :: RoadMap
gTest4 = [("0","1",4),("2","3",2), ("0","2",1), ("0", "3", 1), ("1", "2", 1), ("1", "3", 1)]

gTest5 :: RoadMap
gTest5 = [
    ("0","1",1),
    ("1","2",1),
    ("0","2",2),    -- Direct path from 0 to 2
    ("2","3",1),
    ("1","3",2)]