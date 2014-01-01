module Budget where
import Data.Either;
import Data.List;
import Data.List.Split;
import Text.CSV (CSV,Field,Record);
import Data.Char;
import Data.Time.Calendar;
import Data.Time.LocalTime;
import Data.Time.Format;
import System.Locale;
import Text.Printf (printf);

-- Data definitions
data BudgetRecord = BudgetRecord {
    brDate :: LocalTime
  , brOwners :: [Person]
  , brPurchaser :: Person
  , brAmount :: Double
  , brDescription :: String
  , brTags :: [Tag]
  } deriving (Show,Eq)

type Budget = [BudgetRecord]
type Tag = String
type Person = String

mkBudgetRecord :: BudgetRecord
mkBudgetRecord = BudgetRecord (LocalTime (ModifiedJulianDay 0) midnight) [] "" 0 "" []

data ParseError = ParseError String deriving (Show,Eq)

-- Compile information about Budget

-- | List all tags contained in Budget
tags :: Budget -> [Tag]
tags = tags' []

-- | Accumulator version of tags
tags' :: [Tag] -> Budget -> [Tag]
tags' acc [] = sort $ Data.List.nub acc
tags' acc (x:xs) = tags' (acc ++ (brTags x)) xs

-- | Sum of all amounts in Budget
sum :: Budget -> Double
sum = sum' 0

-- | Accumulator version of sum
sum' :: Double -> Budget -> Double
sum' acc [] = acc
sum' acc (x:xs) = sum' (acc + (brAmount x)) xs


-- | Extract revenues from Budget
revenues :: Budget -> Budget
revenues b = [ x | x <- b, brAmount x < 0 ]

-- | Extract expenses from Budget
expenses :: Budget -> Budget
expenses b = [ x | x <- b, brAmount x > 0 ]

-- | Extract entries with specific tag from Budget
tagged :: Tag -> Budget -> Budget
tagged t b = [ x | x <- b, t `elem` (brTags x) ]

-- | Extract entries purchased by one person
purchased :: Person -> Budget -> Budget
purchased p b = [ x | x <- b, p == (brPurchaser x) ]

-- | Extract entries owned by one person
owned :: Person -> Budget -> Budget
owned o b = [ x | x <- b, o `elem` (brOwners x) ]

-- | Format $ amount
fmt :: Double -> String
fmt a = printf "%8.2f $" a

-- Create budget from CSV
mkBudgetFromCSV :: CSV -> Either [ParseError] Budget
mkBudgetFromCSV [] = Right []
mkBudgetFromCSV csv
  | length errs > 0 = Left errs
  | otherwise       = Right suc
  where
    res = map mkRecordFromCSV [l | l <- csv, validCSVLine l] --Filter-out invalid lines
    errs = lefts res
    suc = rights res

validCSVLine :: [Field] -> Bool
validCSVLine [] = False   --Empty line
validCSVLine [""] = False --Empty line
validCSVLine (x:_)
  | (head x) == '#' = False --Comment line
  | otherwise       = True

mkRecordFromCSV :: Record -> Either ParseError BudgetRecord
mkRecordFromCSV [d,o,p,a,de,t]
  = setDate d mkBudgetRecord >>= setOwners o >>=
    setPurchaser p >>= setAmount a >>=
    setDescription de >>= setTags t
mkRecordFromCSV l = Left (ParseError ("Wrong number of fields: " ++ (show l)))

-- Field parsing
setDate :: String -> BudgetRecord -> Either ParseError BudgetRecord
setDate d br = case res of
  Just x -> Right (br { brDate = x })
  Nothing -> Left (ParseError ("Could not parse date: " ++ d))
  where
    res = parseTime defaultTimeLocale "%Y-%m-%d" d

setOwners :: String -> BudgetRecord -> Either ParseError BudgetRecord
setOwners ostr br = Right (br { brOwners = o })
  where o = [ trim t | t <- splitOn "," ostr, t /= "" ]

setPurchaser :: String -> BudgetRecord -> Either ParseError BudgetRecord
setPurchaser p br = Right (br { brPurchaser = p })

setAmount :: String -> BudgetRecord -> Either ParseError BudgetRecord
setAmount x br = case amt of
  []       -> Left (ParseError ("Could not parse amount: " ++ x))
  [(a,"")] -> Right (br { brAmount = a })
  rds      -> Left (ParseError ("Could not completely parse amount: " ++ (show rds)))
  where amt = reads x :: [(Double,String)]

setDescription :: String -> BudgetRecord -> Either ParseError BudgetRecord
setDescription d br = Right (br { brDescription = d })

setTags :: String -> BudgetRecord -> Either ParseError BudgetRecord
setTags tagstring br = Right (br { brTags = tgs })
  where tgs = [ trim t | t <- splitOn "," tagstring, t /= "" ]

-- Utility
trim :: String -> String
trim t = ((dropWhile isSpace) . (dropWhileEnd isSpace)) t
