import System.IO
import Data.Char
import Data.List
import Data.Ord
import Control.Monad
import Text.JSON (Result(..))
import qualified Text.JSON as JSON
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

type StringMap = [(String, String)]

lcsubs :: (Eq a) => [a] -> [a] -> [a]
lcsubs xs ys = maximumBy (comparing length) $
		-- Concat all the common substrings of xs in ys and vice-versa
		concatMap (csubs ys) (tails xs) ++
			concatMap (csubs xs) (drop 1 $ tails ys)
	where
	-- Generate the common substrings between xs and ys
	csubs xs ys = scanr consOrDrop [] $ zip xs ys
	consOrDrop (x, y) acc | x == y = x:acc
	consOrDrop _ _ = []

parseJsonObjects :: String -> Result [StringMap]
parseJsonObjects str =
	mapM (fmap JSON.fromJSObject . JSON.decode) (lines str)

-- Sort and then groupBy is exactly the same complexity as building a Map
-- with (++) as the combine function (n log n + n worst case)
totalGroupBy :: (a -> a -> Ordering) -> [a] -> [[a]]
totalGroupBy cmp xs = groupBy (\x -> (EQ==) . cmp x) (sortBy cmp xs)

bucketOnMfg :: [StringMap] -> [Maybe String] -> Map String [StringMap]
bucketOnMfg l mfgs =
	foldl' (\m group ->
		let mfgG = mfgForGroup group in
			-- Find normalized manufacturer for this group
			case find (\mfg ->
				-- Get length of smallest mfg string and common substring
				-- Everything here is a Maybe, handle errors appropriately
				let minL = maybeMinLength mfg mfgG
				    llcsub = liftM2 llcsubs (mfgForGroup group) mfg in
					-- If the common substring length is >= the length of
					-- the smallest mfg string, then this mfg is a good match
					nothingIsFalse $ liftM2 (>=) llcsub minL
			) mfgs of
				Just (Just mfg) -> Map.insertWith (++) mfg group m
				_ -> m
	) Map.empty groupOnMfg
	where
	nothingIsFalse Nothing = False
	nothingIsFalse (Just x) = x
	maybeMinLength xs ys = liftM2 min (fmap length xs) (fmap length ys)
	llcsubs xs ys = length $ lcsubs xs ys
	mfgForGroup group = case lookup "manufacturer" $ head group of
		Just "" -> Nothing -- Ignore empty manufacturer strings
		x -> x
	-- Pre-group items to reduce number of expensive fuzzy matches
	groupOnMfg = totalGroupBy (comparing $ lookup "manufacturer") l

queryTokens :: String -> Set String
queryTokens s = queryTokens' (map toLower s) Set.empty
	where
	queryTokens' str set =
		case dropWhile (not . isAlphaNum) str of
			[] -> set
			cleanStr ->
				let (token, rest) = span isAlphaNum cleanStr in
					queryTokens' rest (Set.insert token set)

main :: IO ()
main = do
	listings <- readFile "listings.txt"
	products <- readFile "products.txt"
	-- First, decode the JSON into association lists. Handle errors
	case mapM parseJsonObjects [listings, products] of
		Ok [l,p] ->
			let pByMfg = productMap p in
				print $ bucketOnMfg l (Map.keys pByMfg)
		Error s -> hPutStrLn stderr s
		_ -> error "coding error"
	where
	productMap = foldl' (\m x ->
			case extract x of
				Just (name,query) -> Map.insertWith (++)
					(lookup "manufacturer" x) [(name, queryTokens query)] m
				Nothing -> m
		) Map.empty
	extract x = liftM2 (,) (lookup "product_name" x)
		(lookup "model" x ?++ Just " " ?++ lookup "family" x)
	(?++) = liftM2 (++)
