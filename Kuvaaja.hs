{-#LANGUAGE TupleSections #-}

import Data.Char
import Data.List
import Control.Monad.Trans.State
import Control.Monad

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
parseBracket  ('{':xs) = if (takeLast xs /= '}') then error "Descriptor doesn't end with `}`"
                         else parseBracket' $ init xs
                        where takeLast (_:ys) = if length ys == 1 then ys!!0 else takeLast ys
parseBracket  _ = error "Descriptor doesn't start with `{`"

parseEQN :: Input -> [(String, [String])]
parseEQN = map ((\(a,b) -> (a, parseBracket b)))
          .splitEQUAL

splitEQUAL :: Input -> [(String, String)]
splitEQUAL = map (f.(break (=='=')))
          where f (a,b) = (trimLRspaces a, trimLRspaces.(drop 1)$b)

newtype SYM = SYM {eq::(String, String)}
instance Show SYM where
    show (SYM(a,b)) = a++"="++b
query :: String -> State [SYM] (Maybe String)
query x = state $ f x
         where f x xs = (,xs)$snd.eq <$> find ((==x).fst.eq) xs

replaceSym :: String -> [SYM] -> String
replaceSym x s = case evalState (query x) s of
                   Just res -> res
                   Nothing -> error$"Cannot resolve symbol "++x
replace :: String -> State [SYM] String
replace x = state $ f x
           where f a@(k:xs) s = (,s)$if k=='*' then replaceSym xs s else a

--evalRHS :: (String, [String]) -> [SYM] -> (String, String)
--evalRHS :: (String, [String])
evalRHS x = state $ f x
           where f (a,b) s = (,s).(a,).fst $ foldM (flip runState) s (map replace b)

mem :: [SYM]
mem = [SYM("a", "{b}"), SYM("c", "{d}")]
-- parse :: String -> [SYM] ->
eval str = f str
           where f str = (,s)
                        .(map ((flip evalState s).evalRHS)) -- Replace `*(VARNAME)` to it's content
                        .parse$str
                 loadSym = map$ SYM.(\(a,b) -> (a, '{':intercalate ", " (filter ((/='*').(!!0)) b)++"}"))
                 s = loadSym.parse$str
parse str = parseEQN.splitLines
           $str

--exec' s = (flip evalState s).eval
--exec = (flip evalState []).eval
--
--main = do
--    x <- getLine
--    print.exec$x
