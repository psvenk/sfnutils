import System.Environment
import System.Directory
import qualified Data.Map as Map
import Control.Monad.Trans.State
import Data.Char
import Text.Printf
import Data.Maybe
import Data.List
import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as BSU

data ShortName = ShortName
    { shortNameName  :: String
    , shortNameExt   :: String
    } deriving (Eq, Ord)
instance Show ShortName where
    show (ShortName name ext) = printf "%-8s %-3s" name ext

-- Convert a |String| to a |ShortName| (using |State| monad)
makeShortName :: String -> State (Map.Map String Int) ShortName
makeShortName name = get >>= \m -> let (sname, m') = makeShortName' name m
                                    in put m' >> return sname

-- Convert a |String| to a |ShortName| (non-monadic)
makeShortName'         :: String -> Map.Map String Int ->
    (ShortName, Map.Map String Int)
makeShortName' name m  =
    let (fname, ext, modified)   = sanitizeName name
        fname6                   = take 6 fname
        (num, m')                =
            if modified
               then mapFst (fromMaybe 1) $ Map.insertLookupWithKey
                    (const $ const $ (+1)) fname6 2 m
               else (1, m)
        fname'                   =
            if modified
               then (if num < 10 then fname6 else take 5 fname6)
                    ++ "~" ++ show num
               else fname
     in (ShortName fname' ext, m')

-- Sanitize a file name for 8.3, returning a tuple |(name, ext, modified)|
sanitizeName       :: String -> (String, String, Bool)
sanitizeName name  =
    let  transform x             =
            case () of
              _ | not $ isAscii x  ->
                  -- Add one underscore for each byte in the character |x|
                  (replicate (BS.length $ BSU.fromString [x]) '_', True)
                | x == '+'         -> ("_",          True)
                | otherwise        -> ([toUpper x],  False)
         -- Replace non-ASCII and |'+'| with |'_'| and convert to uppercase
         (name', mod1)           = map2 (foldl (++) "", or) $
             unzip $ map transform name
         (fname, ext)            = splitAtLastMatch (=='.') name'
         -- Remove |' '| and |'.'|
         ((fname', ext'), mod2)  = mapSnd or $ unzip2 $
             mapBoth (filter' $ \x -> x /= ' ' && x /= '.') (fname, ext)
         modified                = length fname' > 8 || length ext' > 3
             || mod1 || mod2
     in (take 8 fname', take 3 ext', modified)

mapFst              :: (a -> a') -> (a, b) -> (a', b)
mapFst f (a, b)     = (f a, b)
mapSnd              :: (b -> b') -> (a, b) -> (a, b')
mapSnd f (a, b)     = (a, f b)
mapBoth             :: (a -> a') -> (a, a) -> (a', a')
mapBoth f (a, b)    = (f a, f b)
map2                :: (a -> a', b -> b') -> (a, b) -> (a', b')
map2 (f, g) (a, b)  = (f a, g b)
unzip2                   :: ((a, b), (c, d)) -> ((a, c), (b, d))
unzip2 ((a, b), (c, d))  = ((a, c), (b, d))

-- Like |filter| but returning a |Bool| which is |True| iff any items failed
-- the predicate
filter'           :: (a -> Bool) -> [a] -> ([a], Bool)
filter' f (x:xs)  =
    case filter' f xs of
      (xs', True)   -> (if f x then x:xs' else xs', True)
      (xs', False)  -> if f x then (x:xs', False) else (xs', True)
filter' f []      = ([], False)

-- Split the list at the last value satisfing the predicate.
-- Worst-case time complexity: $\mathcal{O}(n)$
splitAtLastMatch           :: (a -> Bool) -> [a] -> ([a], [a])
splitAtLastMatch f (x:xs)  =
    case splitAtLastMatch f xs of
      (_,   [])  -> if f x then ([], xs) else (x:xs, [])
      (hs,  ts)  -> (x:hs, ts)
splitAtLastMatch f []      = ([], [])

getFiles       :: String -> IO [ShortName]
getFiles path  = do
    names <- listDirectory path
    -- Traverse the list |names| to thread the state and run
    -- |makeShortName| on each element of the list
    return $ evalState (traverse makeShortName names) Map.empty

main :: IO ()
main = do
    args     <- getArgs
    let path = case args of
                 x : _  -> x
                 _      -> "."
    files    <- getFiles path
    mapM_ (putStrLn . show) $ sort files
