{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
import qualified Data.List
import Foreign (nullPtr)
import Data.Maybe (isJust)
import Data.Type.Bool (Not)
import Distribution.Simple.Program.HcPkg (list)
--import qualified Data.Array
--import qualified Data.Bits

-- PFL 2024/2025 Practical assignment 1

-- Uncomment the some/all of the first three lines to import the modules, do not change the code of these lines.

type City = String
type Path = [City]
type Distance = Int

type RoadMap = [(City,City,Distance)]

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
    | isJust dist = pathDistanceAux (pathDistance graph (tail path)) dist
    | otherwise = Nothing 
    where dist = distance graph (head path) (head (tail path))

------------------------------------------------------------------------------

--Auxiliary function that pairs each city to the number of it´s adjacents
cityAdjacent :: RoadMap -> [City] -> [(City,Int)] 
cityAdjacent _ [] = []
cityAdjacent graph listOfCities =  (head listOfCities , length(adjacent graph (head listOfCities))) : cityAdjacent graph (tail listOfCities) 

--Auxiliary function that returns the cities with maximum adjacent length
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

------------------------------------------------------------------------------
shortestPath :: RoadMap -> City -> City -> [Path]
shortestPath = undefined

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