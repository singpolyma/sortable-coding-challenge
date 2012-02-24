import System.IO
import Data.Char
import Data.List
import Data.Ord
import Data.Maybe
import Control.Monad
import Text.JSON (Result(..))
import qualified Text.JSON as JSON
import Data.Map (Map,(!))
import qualified Data.Map as Map

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
					fromMaybe False $ liftM2 (>=) llcsub minL
			) mfgs of
				Just (Just mfg) -> Map.insertWith (++) mfg group m
				_ -> m
	) Map.empty groupOnMfg
	where
	maybeMinLength xs ys = liftM2 min (fmap length xs) (fmap length ys)
	llcsubs xs ys = length $ lcsubs xs ys
	mfgForGroup group = lookupMfg $ head group
	-- Pre-group items to reduce number of expensive fuzzy matches
	groupOnMfg = totalGroupBy (comparing lookupMfg) l
	lookupMfg x = fmap (map toLower) $ case lookup "manufacturer" x of
		Just "" -> lookup "title" x
		Nothing -> lookup "title" x
		x -> x

cleanString :: String -> String
cleanString s =
	concat $ addSpaces (concat $ queryTokens' (map toLower s) []) []
	where
	-- Add a space after every number that does not already have on after it
	addSpaces [] xs = xs
	addSpaces str xs =
		let (noNum, noRest) = break isDigit str in
			case span isDigit noRest of
				([], rest) -> noNum : addSpaces rest xs
				(num, rest) ->
					case addSpaces rest xs of
						recurs@((' ':_):_) -> noNum:num:recurs
						recurs -> noNum:num:" ":recurs
	-- Strip non-alphanumeric characters
	-- Leave a space between numbers and other stuff
	queryTokens' str xs =
		case dropWhile (not . isAlphaNum) str of
			[] -> xs
			cleanStr ->
				let (token, rest) = span isAlphaNum cleanStr
				    recurs = queryTokens' rest xs in
					token : if isDigit (last token) then " ":recurs else recurs

stripPrefix' :: Eq a => [a] -> [a] -> [a]
stripPrefix' prefix xs = fromMaybe xs (stripPrefix prefix xs)

matchOneMfg :: [(String,String,String)] -> DictMap -> StringMap -> DictMap
matchOneMfg mfgProducts m listing =
	case filter (\(_,query,_) ->
		-- Filter by model
		fromMaybe False $ fmap (isInfixOf query) cleanTitle
	) mfgProducts of
		[] -> continueWithStrip mfgProducts
		[(name,_,_)] -> ins name m
		xs -> continueByFamily xs
	where
	continueWithStrip xs =
		case filter (\(_,query,_) ->
			-- Filter by model, strip common prefixes
			fromMaybe False $
				fmap (isInfixOf $ stripPrefixes query) cleanTitle
		) xs of
			[] -> m -- No product matched
			[(name,_,_)] -> ins name m
			ys -> continueByFamily ys
	continueByFamily xs =
		case filter (\(_,_,query) ->
			-- Filter by family
			length query > 0 &&
				fromMaybe False (fmap (isInfixOf query) cleanTitle)
		) xs of
			[] -> m -- No product matched
			[(name,_,_)] -> ins name m
			ys -> -- Still more than one match. Take the longest model
				let (name,_,_) = maximumBy (\(_,a,_) (_,b,_) ->
						comparing length a b
					) ys in ins name m
	stripPrefixes s = stripPrefix' "dsc" $ stripPrefix' "dslr" s
	ins name = Map.insertWith (++) name [listing]
	cleanTitle = fmap cleanString (lookup "title" listing)

matchAllMfg :: Map (Maybe String) [(String,String,String)] ->
               DictMap -> DictMap
matchAllMfg pByMfg = Map.foldrWithKey (\mfg grp m ->
		-- Build up a map from product_name to list of listings
		Map.union m $ foldl' (matchOneMfg (pByMfg ! Just mfg)) Map.empty grp
	) Map.empty

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
					JSON.toJSObject [
						("product_name", JSON.showJSON name),
						("listings", JSON.showJSON $ map JSON.toJSObject listings)
					] : acc
				) [] res in
					mapM_ (putStrLn . JSON.encode) json
					-- The following is just an output hack that makes eyeballing
					-- the results easier during debugging
					-- sequence_ $ Map.foldrWithKey (\name listings acc -> acc ++ [putStrLn name] ++ map (\l -> putStrLn (fromJust $ lookup "title" l)) listings ++ [putStrLn ""]) [] res
		Error s -> hPutStrLn stderr s
		_ -> error "coding error"
	where
	-- Extract the data we need from products
	-- and put it all in a nice map key'd on manufacturer
	productMap = foldl' (\m x ->
			case extract x of
				Just (family,model,name) -> Map.insertWith (++)
					(fmap (map toLower) (lookup "manufacturer" x))
						[(name, cleanString model, cleanString family)] m
				Nothing -> m
		) Map.empty
	-- Extract the data we need from a product
	extract x = liftM2 ((,,) (fromMaybe [] $ lookup "family" x))
		(lookup "model" x) (lookup "product_name" x)
