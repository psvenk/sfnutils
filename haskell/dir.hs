import qualified Data.Map as Map (Map, empty, insertLookupWithKey)
import qualified Data.ByteString as BS (length)
import qualified Data.ByteString.UTF8 as BSU (fromString)
import Data.List (mapAccumL, uncons, sort)
import Data.Char (isAscii, toUpper)
import Data.Maybe (fromMaybe)
import System.Environment (getArgs)
import System.Directory (listDirectory)
import Text.Printf (printf)

data ShortName = ShortName
    { shortNameName  :: String
    , shortNameExt   :: String
    } deriving (Eq, Ord, Show, Read)

bimap (f, g) (a, b) = (f a, g b)
dup x = (x, x)
unzipT2 ((a, b), (c, d))  = ((a, c), (b, d))

-- Split the list at the last value satisfing the predicate.
-- Worst-case time complexity: $\mathcal{O}(n)$
splitLast           :: (a -> Bool) -> [a] -> ([a], [a])
splitLast f (x:xs)  =
    case splitLast f xs of
      (_,   [])  -> if f x then ([], xs) else (x:xs, [])
      (hs,  ts)  -> (x:hs, ts)
splitLast _ []      = ([], [])

-- Like |filter| but also returning a |Bool| which is |True| iff any items
-- failed the predicate (so that the list is only traversed once)
filter'           :: (a -> Bool) -> [a] -> ([a], Bool)
filter' f (x:xs)  =
    case filter' f xs of
      (xs', True)   -> (if f x then x:xs' else xs', True)
      (xs', False)  -> if f x then (x:xs', False) else (xs', True)
filter' _ []      = ([], False)

-- Like |take| but also returning a |Bool| which is |True| iff the array
-- was truncated (so that the list is only traversed once)
take'           :: Int -> [a] -> ([a], Bool)
take' n (x:xs)  = if n <= 0 then ([], True)
                            else let (xs', b) = take' (n-1) xs in (x:xs', b)
take' _ []      = ([], False)

-- Transforms a character for 8.3 sanitization; the |Bool| return value
-- is |True| iff any modification other than uppercasing occurred
transform :: Char -> (String, Bool)
transform c
  -- Add one underscore for each byte taken by the character
  | not $ isAscii c  = (replicate (BS.length $ BSU.fromString [c]) '_', True)
  | c == '+'         = ("_", True)
  | otherwise        = ([toUpper c], False)

-- Sanitize a file name for 8.3, returning a tuple |(name, ext, modified)|
sanitizeName       :: String -> (String, String, Bool)
sanitizeName name  = (fname'', ext'', modified)
    where (name', mod1) = bimap (concat, or) $ unzip $
              map transform name
          (fname, ext)  = splitLast (=='.') name'
          ((fname', ext'), mod2) = bimap (id, uncurry (||)) $ unzipT2 $
              bimap (dup $ filter' $ \x -> x /= ' ' && x /= '.') (fname, ext)
          ((fname'', ext''), mod3) = bimap (id, uncurry (||)) $ unzipT2 $
              bimap (take' 8, take' 3) (fname', ext')
          modified = mod1 || mod2 || mod3

-- Convert a |String| to a |ShortName| with a |Map.Map String Int| keeping
-- state
makeShortName         :: Map.Map String Int -> String ->
    (Map.Map String Int, ShortName)
makeShortName m name  =
    let (fname, ext, modified)   = sanitizeName name
        fname6                   = take 6 fname
        (num, m')                =
            if modified
               then bimap (fromMaybe 1, id) $ Map.insertLookupWithKey
                    (const $ const (+1)) fname6 2 m
               else (1, m)
        fname'                   =
            if modified
               then (if num < 10 then fname6 else take 5 fname6)
                    ++ "~" ++ show num
               else fname
     in (m', ShortName fname' ext)

getFiles :: String -> IO [ShortName]
getFiles = (snd . mapAccumL makeShortName Map.empty <$>) . listDirectory

-- Custom implementations of |Show| are apparently an antipattern
-- because they violate the law |read . show = id|
-- (see \url{https://www.stephendiehl.com/posts/strings.html})
shortNameToString                      :: ShortName -> String
shortNameToString (ShortName name ext) = printf "%-8s %-3s" name ext

main :: IO ()
main = getArgs >>= getFiles . maybe "." fst . uncons >>=
    mapM_ (putStrLn . shortNameToString) . sort
