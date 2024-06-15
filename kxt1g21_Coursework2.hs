import Data.List(tails)

main :: IO ()
main = do
    let atom = [(2,3), (7,3), (4,6), (7,8)]
        atom2 = [(7,2), (3,3), (4,4), (5,7)]

        grid = 8
        atomsNo = 4

        calInteraction = calcBBInteractions grid atom
        calInteraction2 = calcBBInteractions grid atom2

        notComplete = [((North,1),Path(South,1)),((North,5),Path(South,5))]
        
        solve = solveBB atomsNo calInteraction
        solve2 = solveBB atomsNo notComplete

    putStrLn $ "Interactions of "++ show atom ++ ": " ++ show calInteraction

    putStrLn $ "Interactions of "++ show atom2 ++ ": " ++ show calInteraction2

    putStrLn $ "Atoms Found in Challenge 1 Output: " ++ show solve
    putStrLn $ "Atoms Found in "++ show notComplete ++ ": " ++ show solve2


type Pos = (Int, Int) 
type Atoms = [Pos]
data Side = North | East | South | West deriving (Show, Eq, Ord) 
type EdgePos = (Side, Int)
data Marking = Absorb | Reflect | Path EdgePos deriving (Show, Eq) 
type Interactions = [ (EdgePos , Marking) ]   

--the rays moving direction: Up, Down, Right, Left
data RayDirection = U | D | R | L deriving (Show, Eq) 

-----Challenge 1-----
--Function to get all interactions of North, East, South, West
calcBBInteractions :: Int -> Atoms -> Interactions
calcBBInteractions gridSize atoms =
    [((North,i),interactionForEdge (North,i) atoms (i,0) gridSize D) | i <- [1..gridSize]]++           --Ray that start at North will travel Down
    [((East,i),interactionForEdge (East,i) atoms (gridSize+1,i) gridSize L) | i <- [1..gridSize]]++    --Ray that start at East will travel Left
    [((South,i),interactionForEdge (South,i) atoms (i,gridSize+1) gridSize U) | i <- [1..gridSize]]++  --Ray that start at South will travel Up
    [((West,i),interactionForEdge (West,i) atoms (0,i) gridSize R) | i <- [1..gridSize]]               --Ray that start at West will travel Right

--Main function for rays' interaction with atoms
--The function will need the input of: 
--Initial Side of the Ray, Atoms in the grid, Current coordinate of the ray in the grid, Grid's size, and Direction of the ray is travelling
interactionForEdge :: EdgePos -> Atoms -> Pos -> Int -> RayDirection -> Marking
interactionForEdge (side, pos) atoms (ex,ey) gridSize dir =  
    --check where the ray is travelling
    case dir of
        D -> upDown (atomsFound False) 1 South  
        U -> upDown (atomsFound True) (-1) North
        R -> leftRight (atomsFound False) 1 East
        L -> leftRight (atomsFound True) (-1) West
    where
        --Get the atoms that will get striked by the ray
        atomsFound :: Bool -> Atoms
        atomsFound checkOrder
            --get atoms that are nearest to the side
            | dir == D || dir == U = filter (\(_,y) -> y==minMaxValue) filteredAtoms
            | otherwise = filter (\(x,_) -> x==minMaxValue) filteredAtoms
            where
                --get atoms that are in three columns/rows of the ray
                filteredAtoms = filter (\(x, y) -> getAtomsNearRay x y) atoms

                getAtomsNearRay x y
                    | dir == U || dir == D = checkOrderFunc ey y && (ex - 1 == x || ex + 1 == x || ex==x)
                    | otherwise = checkOrderFunc ex x && (ey - 1 == y || ey + 1 == y || ey==y)

                checkOrderFunc val1 val2
                    | checkOrder == True = val1 > val2
                    | otherwise = val2 > val1

                --sort atoms that are nearest to the side
                minMaxValue
                    | dir == D = minimum $ map snd filteredAtoms
                    | dir == U = maximum $ map snd filteredAtoms
                    | dir == R = minimum $ map fst filteredAtoms
                    | otherwise = maximum $ map fst filteredAtoms
        
        --function that will check whether the ray will be absorb / reflect / deflect
        leftRight getAtom coor side
          | null getAtom = Path (side, ey)
          | any (\(_, y) -> y == pos) getAtom = Absorb
          | length getAtom == 2 || (side==East && fst (head getAtom) ==1) || (side==West&&fst (head getAtom) ==gridSize) = Reflect
          | ey - snd (head getAtom) == 1 = interactionForEdge (South, fst (head getAtom)-coor) atoms (fst (head getAtom)-coor,ey) gridSize D
          | otherwise = interactionForEdge (North, fst (head getAtom)-coor) atoms (fst (head getAtom)-coor,ey) gridSize U

        upDown getAtom coor side
          | null getAtom = Path (side,ex)
          | any (\(x, _) -> x == pos) getAtom = Absorb
          | length getAtom == 2 || (side==South && snd (head getAtom) ==1) || (side==North&&snd (head getAtom) ==gridSize) = Reflect
          | ex - fst (head getAtom) == 1 = interactionForEdge (East, snd (head getAtom)-coor) atoms (ex, snd (head getAtom)-coor) gridSize R
          | otherwise = interactionForEdge (West, snd (head getAtom)-coor) atoms (ex, snd (head getAtom)-coor) gridSize L

-----Challenge 2-----
--Possible combinations of atoms
combinations :: Int -> Atoms -> [Atoms]
combinations 0 _ = [[]]
combinations val atom = [a:as | a:atom' <- tails atom, as <- combinations (val-1) atom']

--Find all possible atoms from interactions
solveBB :: Int -> Interactions -> [Atoms]
solveBB val interactions =
    filter (\atoms -> all(`elem` calcBBInteractions getGridSize atoms) interactions) (combinations val allAtoms) 
  where
    --Get max value from the interactions to find the grid size
    getGridSize = maximum [i | (_, Path (_,i)) <- interactions , ((_,i),_) <- interactions] 
    --Create all atoms in the grid
    allAtoms = [(x, y) | x <- [1..getGridSize], y <- [1..getGridSize]]
