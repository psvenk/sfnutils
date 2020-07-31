import System.Environment
import System.Directory
import qualified Data.Map as Map
import Control.Monad.Trans.State
import Data.Char
import Text.Printf

data ShortName = ShortName
    { shortNameName  :: String
    , shortNameExt   :: String
    }
instance Show ShortName where
    show (ShortName name ext) = printf "%-8s %-3s" name ext

-- Convert a |String| to a |ShortName|
makeShortName       :: String -> State (Map.Map String Integer) ShortName
makeShortName name  = do
    put Map.empty
    let (fname, ext, modified) = sanitizeName name
    -- TODO add |'~'| and number if modified
    -- |m <- get|
    -- |modify (\m -> ...)|
    return $ ShortName fname ext

-- Sanitize a file name for 8.3, returning a tuple |(name, ext, modified)|
sanitizeName       :: String -> (String, String, Bool)
sanitizeName name  =
    let  transform x   = if not $ isAscii x || x == '+' then '_' else toUpper x
         name'         = map transform name
         (fname, ext)  = splitAtLastMatch (=='.') name'
         modified      = length fname > 8 || length ext > 3
             || name' /= map toUpper name
     in (take 8 fname, take 3 ext, modified)

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
    -- Lists implement |Traversable|, so we can use |mapM| to thread a single
    -- state (|Map.empty|) and run |makeShortName| on every element of the list.
    return $ evalState (mapM makeShortName names) Map.empty

main :: IO ()
main = do
    args     <- getArgs
    let path = case args of
                 x : _  -> x
                 _      -> "."
    files    <- getFiles path
    -- TODO sort |files|
    mapM_ (putStrLn . show) files
