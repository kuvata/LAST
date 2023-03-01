{-#LANGUAGE TupleSections #-}
{-# LANGUAGE DerivingVia #-}

import Data.Char
import Data.List
import Control.Monad.Trans.State
import Control.Monad
import Data.Bifunctor (second)
import Data.Tuple.Extra (dupe)



newtype Descriptor = Descriptor {content::[String]}
                        deriving (Eq)
                        deriving (Semigroup, Monoid) via ([String])
type EQN = (String, Descriptor)
data SYM = Selector{eqn::EQN}
         | Object{eqn::EQN}
          deriving (Eq)
instance Show SYM where
    show (Selector (a, b)) = a++"=S"++show b
    show (Object (a, b)) = a++"=O"++show b
    --show x = a++"="++show b
    --        where (a,b) = eqn x
instance Show Descriptor where
    show (Descriptor xs) = "{"++(intercalate ", " xs)++"}"
mkDescriptor :: [String] -> Descriptor
mkDescriptor = Descriptor . nub
--newtype SYM = SYM {eq::SYM} deriving (Eq)
--instance Show SYM where
--    show (SYM(a,b)) = a++"="++show b
type MEM = [SYM]


trimLRspaces = reverse.(dropWhile isSpace)
              .reverse.(dropWhile isSpace)
               

type Input = [String]
splitLines :: String -> Input
splitLines [] = []
splitLines s = a : splitLines (drop 1 b)
         where (a,b) = break (=='\n') s

parseBracket' :: String -> [String]
parseBracket' [] = []
parseBracket' s = trimLRspaces a : parseBracket' (drop 1 b)
                 where (a,b) = break (==',') s
parseBracket  :: String -> [String]
parseBracket  "{}" = []
parseBracket  ('{':xs) = if takeLast xs /= '}' then error "Descriptor doesn't end with `}`"
                         else parseBracket' $ init xs
                        where takeLast (_:ys) = if length ys == 1 then head ys else takeLast ys
parseBracket  _ = error "Descriptor doesn't start with `{`"


assignSYM :: (String, Descriptor) -> SYM
assignSYM x@(_, xs)
            | "..." `elem` xs' = Object x
         -- | "..." `elem` (map (take 3) xs) = Object x
            | otherwise = Selector x
             where xs' = content xs

parseEQN :: Input -> [SYM]
parseEQN = map (assignSYM.(second (Descriptor . parseBracket)))
          .splitEQUAL

splitEQUAL :: Input -> [(String, String)]
splitEQUAL = map (f.(break (=='=')))
          where f (a,b) = (trimLRspaces a, trimLRspaces.(drop 1)$b)

splitBrackets :: String -> [String]
splitBrackets [] = []
splitBrackets s = if h=="{" then trimLRspaces (h++a):splitBrackets b
                  else singleton s
                 where (a,b) = break (=='{') s'
                       (h,s') = (splitAt 1).trimLRspaces$s

query :: String -> State MEM (Maybe Descriptor)
query x = state $ f x
         where f x xs = (,xs)$snd.eqn <$> find ((==x).fst.eqn) xs

replaceSym :: String -> MEM -> Descriptor
replaceSym x s = case evalState (query x) s of
                   Just res -> res
                   Nothing -> error$"Cannot resolve symbol "++x
replace :: String -> State MEM [String]
replace x = state $ f x
           where f a@(k:xs) s = let x' = (dropWhile (not.(=='*')) a) in (,s)
                               $if (take 1 x')=="*"
                                then content$replaceSym (drop 1 x') s-- ++"="++x'
                                else singleton a

--evalRHS :: (String, [String]) -> [SYM] -> (String, String)
--evalRHS :: (String, [String])
evalRHS :: SYM -> State MEM EQN
evalRHS x = state $ f (eqn x)
--f :: EQN -> [SYM] -> ((String, [String]), [SYM])
           where f (a,Descriptor b) s = (,s).(a,).mkDescriptor.fst
                  $foldM (flip runState) s (map replace b)

eval :: MEM -> [SYM] -> [SYM] -- Not outputing `MEM` as it's `snd/state` needs to be further filtered through `loadSym'`
eval s = (map (assignSYM.(`evalState` s).evalRHS)) -- Replace `*(VARNAME)` to it's content

exec :: String -> State MEM MEM
exec str = state$ f str
          where f str s = (\(a,b)->(a, map loadSym' b)).dupe
                         .(eval s).parse$str

loadSym :: [SYM] -> State MEM ()
loadSym = put.map loadSym'
loadSym' :: SYM -> SYM
loadSym' (Selector x) = Selector.(\(a,b) -> (a, Descriptor(filter ((/='*').(!!0)) (content b))))$x
loadSym' (Object x)   = Object.(\(a,b) -> (a, Descriptor(filter f (content b))))$x
                       where f x = ((/='*').(!!0)$x) && x/="..."

parse :: String -> [SYM]
parse str = parseEQN.splitLines
           $str

runNtimes 0 x0 = error "(runNtimes) `n`<1"
runNtimes n x0 = loadSym (parse x0) >>= (\x1 -> runNtimes' n x0)
runNtimes' 1 x0 = exec x0 >>= (\x3 -> return x3)
runNtimes' n x0 = exec x0 >>= (\x2 -> runNtimes' (n-1) x0)
-- TODO: Find better method
repeatEval x0 = map (`evalState` []) (repeatEval' x0)
               where repeatEval' x0 = map runNtimes (iterate (+1) 1) <*> pure x0
stableRes x0 = snd.head.(dropWhile (\(a,b) -> a/=b)) $ zip (repeatEval x0) (drop 1 (repeatEval x0))

run x0 = do
    let res= stableRes x0
    let hist=(takeWhile (/=res) (repeatEval x0))++[res]
    print hist
    putStr "Passes: "
    print $ length hist
    putStr "Result: "
    print res

main = do
    x0 <- getContents
    run x0
