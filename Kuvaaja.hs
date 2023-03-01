{-#LANGUAGE TupleSections #-}

import Data.Char
import Data.List
import Control.Monad.Trans.State
import Control.Monad
import Data.Bifunctor (second)
import Data.Tuple.Extra (dupe)

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

parseEQN :: Input -> [(String, [String])]
parseEQN = map (second parseBracket)
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

newtype Descriptor = Descriptor {content::[String]} deriving (Eq)
instance Show Descriptor where
    show (Descriptor xs) = "{"++(intercalate ", " xs)++"}"
mkDescriptor :: [String] -> Descriptor
mkDescriptor = Descriptor . nub
newtype SYM = SYM {eq::(String, Descriptor)} deriving (Eq)
instance Show SYM where
    show (SYM(a,b)) = a++"="++show b
query :: String -> State [SYM] (Maybe Descriptor)
query x = state $ f x
         where f x xs = (,xs)$snd.eq <$> find ((==x).fst.eq) xs

replaceSym :: String -> [SYM] -> [String]
replaceSym x s = case evalState (query x) s of
                   Just res -> content res
                   Nothing -> error$"Cannot resolve symbol "++x
replace :: String -> State [SYM] [String]
replace x = state $ f x
           where f a@(k:xs) s = let x' = (dropWhile (not.(=='*')) a) in (,s)
                               $if (take 1 x')=="*"
                                then replaceSym (drop 1 x') s-- ++"="++x'
                                else singleton a

--evalRHS :: (String, [String]) -> [SYM] -> (String, String)
--evalRHS :: (String, [String])
evalRHS x = state $ f x
           where f (a,b) s = (,s).(a,).fst
                            $foldM (flip runState) s (map replace b)

mem :: [SYM]
mem = [SYM("a", Descriptor["b"]), SYM("c", Descriptor["d"])]
-- parse :: String -> [SYM] ->
eval str = state$ f str
          where f str s = dupe
                         .(map (SYM.(second mkDescriptor)
                               .(`evalState` s).evalRHS)) -- Replace `*(VARNAME)` to it's content
                         .parse$str
                 --s = loadSym'.parse$str
                genSYM (k,d) = SYM$(k, Descriptor (singleton d))
loadSym' :: (Monad m) => [(String, [String])] -> StateT [SYM] m ()
loadSym' = put.map (SYM.(second Descriptor).(\(a,b) -> (a, (filter ((/='*').(!!0)) b))))
--loadSym :: (Monad m) => [(String, String)] -> StateT [SYM] m ()
--loadSym = put.(map (SYM.(\(a,b) -> (a, '{':b++"}"))))
parse str = parseEQN.splitLines
           $str
--exec = (map (SYM.(\(a,b)->(a, '{':b++"}")))).fst.eval

--exec' s = (flip evalState s).eval
--exec = (flip evalState []).eval

--exec str = (evalState (eval str)).loadSym'$parse str
doubleRun str = do
                    m <- loadSym' $ parse str
                    m <- eval str
                    --m2 <- loadSym' m1
                    m <- eval str
                    m <- eval str
                    return m

--runNtimes 1 x0 = loadSym' (parse x0) >>= (\x1 -> eval x0 >>= (\x2 -> return x2))
runNtimes 0 x0 = error "(runNtimes) `n`<1"
runNtimes n x0 = loadSym' (parse x0) >>= (\x1 -> runNtimes' n x0)
runNtimes' 1 x0 = eval x0 >>= (\x3 -> return x3)
runNtimes' n x0 = eval x0 >>= (\x2 -> runNtimes' (n-1) x0)
-- TODO: Find better method
repeatEval x0 = map (`evalState` []) (repeatEval' x0)
               where repeatEval' x0 = map runNtimes (iterate (+1) 1) <*> pure x0
stableRes x0 = snd.head.(dropWhile (\(a,b) -> a/=b)) $ zip (repeatEval x0) (drop 1 (repeatEval x0))

--main=error""
main = do
    x0 <- getContents
    let res= stableRes x0
    let hist=(takeWhile (/=res) (repeatEval x0))++[res]
    print hist
    putStr "Passes: "
    print $ length hist
    putStr "Result: "
    print res
