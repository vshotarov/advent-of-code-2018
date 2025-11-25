module Main where

import qualified Common
import Data.List.Split (splitOn)
import Data.List (sortOn)
import qualified Data.Map as M

main :: IO ()
main = do
    putStrLn $ "-- Solving day24 --"
    input <- Common.readInput

    -- Parse
    let parsedInput@((ign,igg),inf) = parse input
    putStrLn $ "Parsed input: " ++ (Common.truncateString $ show parsedInput)

    -- Solve
    let safeSimulate = Common.mapTuple (sum . map units . M.elems . snd)
                     . last . Common.takeUntil (\s -> isFinished s || isStalemate s)
                     . iterate step
    let answer1 = uncurry (+) $ safeSimulate parsedInput
    let answer2 = uncurry (+) . last . Common.takeUntil ((==0) . snd)
                $ map (safeSimulate . addImmunityAd) [1..]
            where addAd ad' (Group n hp ad i dtyp mods) = Group n hp (ad+ad') i dtyp mods
                  addImmunityAd ad = ((ign, M.map (addAd ad) igg), inf)

    -- Print answers
    putStrLn $ "Part 1: " ++ show answer1
    putStrLn $ "Part 2: " ++ show answer2

type Modifiers = ([String],[String])
data Group = Group { units :: Int, hitPoints :: Int, attackDmg :: Int,
                     initiative :: Int, dmgType :: String, modifiers :: Modifiers }
             deriving Show
type Army = (String, M.Map Int Group)
type State = (Army,Army)

parse :: String -> State
parse input = Common.mapTuple parseArmy . Common.splitOnceOn [""] $ lines input
    where parseModifiers xs = foldr foldMod ([],[]) ms
              where parseModifier [] = error "invalid modifier"
                    parseModifier [_] = error "invalid modifier"
                    parseModifier (m:_:types) = (m, types)
                    foldMod ("weak",types) (w,i) = (w++types,i)
                    foldMod (_,types) (w,i) = (w,i++types)
                    ms = map (parseModifier . map (filter (/=',')) . words)
                       $ splitOn ";" xs
          parseGroup line = Group n hp ad i dtype mods
              where tokens = words line
                    tokensRev = reverse tokens
                    n = read (tokens !! 0)
                    hp = read (tokens !! 4)
                    i = read (tokensRev !! 0)
                    ad = read (tokensRev !! 5)
                    dtype = (tokensRev !! 4)
                    mods = if '(' `elem` line
                           then parseModifiers . takeWhile (/=')') . tail $ dropWhile (/='(') line
                           else ([],[])
          parseArmy [] = error "empty army"
          parseArmy (name:groups) = (init name, M.fromList . zip [0..] $ map parseGroup groups)

effectivePower :: Group -> Int
effectivePower g = units g * attackDmg g

type GroupID = (Int,Int)
type Targeting = (GroupID,GroupID)

targetPhase :: State -> [Targeting]
targetPhase ((_,agrps),(_,bgrps)) = foldl foldHelper [] ordered
    where aGidGrps = map (\(agid,g) -> ((0,agid),g)) $ M.toList agrps
          bGidGrps = map (\(agid,g) -> ((1,agid),g)) $ M.toList bgrps
          gidGrps = aGidGrps ++ bGidGrps
          ordered = sortOn (\(_,g) -> (-effectivePower g, -initiative g)) gidGrps
          getTargets ignore ((aid,_),g) =
              map fst
              . sortOn (\((_,og),dmg) -> (-dmg, -effectivePower og, -initiative og))
              . filter ((>0) . snd)
              . map (\x -> (x,(calcDamage g $ snd x)))
              . filter (\(gid,_) -> not $ gid `elem` ignore) -- not already targeted
              $ filter ((/=aid) . fst . fst) gidGrps -- other army
          foldHelper acc gidGrp
              | null targets = acc
              | otherwise = ((fst gidGrp, fst $ head targets)):acc
              where targets = getTargets (map snd acc) gidGrp

getGroup :: State -> GroupID -> Group
getGroup ((_,agrps),(_,bgrps)) (aid,gid) =
    (if aid == 0 then agrps else bgrps) M.! gid 

calcDamage :: Group -> Group -> Int
calcDamage a b
  | isWeakTo b (dmgType a) = 2*dmg
  | isImmuneTo b (dmgType a) = 0
  | otherwise = dmg
  where dmg = effectivePower a
        isWeakTo g t = t `elem` (fst $ modifiers g)
        isImmuneTo g t = t `elem` (snd $ modifiers g)

takeDamage :: Group -> Int -> Group
takeDamage (Group n hp ad i dtyp mods) dmg = Group n' hp ad i dtyp mods
    where n' = max 0 $ n - dmg `div` hp

attack :: State -> Targeting -> State
attack s@((an,agrps),(bn,bgrps)) (agid,dgid)
    | units a <= 0 = s
    | otherwise = if fst agid == 0
                  then ((an,agrps),(bn, M.insert (snd dgid) d' bgrps))
                  else ((an, M.insert (snd dgid) d' agrps), (bn,bgrps))
    where (a,d) = Common.mapTuple (getGroup s) (agid,dgid)
          d' = takeDamage d $ calcDamage a d

attackPhase :: State -> [Targeting] -> State
attackPhase s = Common.mapTuple (\(n,grps) -> (n, M.filter ((>0) . units) grps))
              . foldl attack s

step :: State -> State
step s = attackPhase s . sortOn ((*(-1)) . initiative . getGroup s . fst) $ targetPhase s

isFinished :: State -> Bool
isFinished ((_,agrps),(_,bgrps)) = M.null agrps || M.null bgrps

isStalemate :: State -> Bool
isStalemate = null . targetPhase

