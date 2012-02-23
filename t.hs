import System.IO
import Data.Char
import Data.List
import Data.Ord
import Control.Monad
import Text.JSON (Result(..))
import qualified Text.JSON as JSON
import Data.Map (Map,(!))
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

type StringMap = [(String, String)]
type DictMap = Map String [StringMap]

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

nothingIsFalse :: Maybe Bool -> Bool
nothingIsFalse Nothing = False
nothingIsFalse (Just x) = x

bucketOnMfg :: [StringMap] -> [Maybe String] -> DictMap
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

matchOneMfg :: [(String,Set String)] -> DictMap -> StringMap -> DictMap
matchOneMfg mfgProducts m listing =
	case find (\(_,query) ->
		-- Get the product from this mfg where the product tokens
		-- are a subset of the listing title tokens
		nothingIsFalse $ fmap (Set.isSubsetOf query)
			(fmap queryTokens (lookup "title" listing))
	) mfgProducts of
		Nothing -> m -- No product matched
		Just (name,_) -> Map.insertWith (++) name [listing] m

matchAllMfg :: Map (Maybe String) [(String, Set String)] ->
               DictMap -> DictMap
matchAllMfg pByMfg lByMfg = Map.foldrWithKey (\mfg grp m ->
		-- Build up a map from product_name to list of listings
		Map.union m $ foldl' (matchOneMfg (pByMfg ! Just mfg)) Map.empty grp
	) Map.empty lByMfg

main :: IO ()
main = do
	listings <- readFile "listings.txt"
	products <- readFile "products.txt"
	-- First, decode the JSON into association lists. Handle errors
	case mapM parseJsonObjects [listings, products] of
		Ok [l,p] ->
			let pByMfg = productMap p
			    res = matchAllMfg pByMfg (bucketOnMfg l (Map.keys pByMfg))
			    -- JSON formatting and output
			    json = Map.foldrWithKey (\name listings acc ->
					(JSON.toJSObject [
						("product_name", JSON.showJSON name),
						("listings", JSON.showJSON $ map JSON.toJSObject listings)
					]) : acc
				) [] res in
					mapM_ putStrLn (map JSON.encode json)
		Error s -> hPutStrLn stderr s
		_ -> error "coding error"
	where
	-- Extract the data we need from products (computing tokens, etc)
	-- and put it all in a nice map key'd on manufacturer
	productMap = foldl' (\m x ->
			case extract x of
				Just (name,query) -> Map.insertWith (++)
					(lookup "manufacturer" x) [(name, queryTokens query)] m
				Nothing -> m
		) Map.empty
	-- Extract the data we need from a product
	extract x = liftM2 (,) (lookup "product_name" x)
		(lookup "model" x ?++ Just " " ?++ lookup "family" x)
	-- Shorthand for: concat monads that have lists in them
	(?++) = liftM2 (++)
