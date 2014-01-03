module Budget.Import where
import Budget;
import Data.Either;
import Data.List;
import Data.List.Split;
import Data.Char;
import Data.Time.Format;
import System.Locale;
import Text.CSV (CSV,Field,Record,parseCSVFromFile);

data ParseError = ParseError String deriving (Show,Eq)

desjardins :: String -> IO Budget
desjardins _ = fail $ "Desjardins format is not yet supported!"

visa :: String -> IO Budget
visa _ = fail $ "VISA format is not yet supported!"

standard :: String -> IO Budget
standard f = do
  csv <- parseCSV f
  case mkBudgetFromStandardCSV csv of
    Left errors -> fail $ "Could not parse Standard file " ++ f ++ ": " ++ (concat (map fmtError errors))
    Right b    -> return b
  where fmtError (ParseError e) = "\n  " ++ e

parseCSV :: String -> IO CSV
parseCSV f = do
  parseResult <- parseCSVFromFile f
  case parseResult of
    Left err -> fail $ "Could not parse CSV file: " ++ (show err)
    Right csv  -> return csv

------------- STANDARD FORMAT ----------------
mkBudgetFromStandardCSV :: CSV -> Either [ParseError] Budget
mkBudgetFromStandardCSV [] = Right []
mkBudgetFromStandardCSV csv
  | length errs > 0 = Left errs
  | otherwise       = Right suc
  where
    res = map mkRecordFromStandardCSV [l | l <- csv, validStandardCSVLine l] --Filter-out invalid lines
    errs = lefts res
    suc = rights res

validStandardCSVLine :: [Field] -> Bool
validStandardCSVLine [] = False   --Empty line
validStandardCSVLine [""] = False --Empty line
validStandardCSVLine (x:_)
  | (head x) == '#' = False --Comment line
  | otherwise       = True

mkRecordFromStandardCSV :: Record -> Either ParseError BudgetRecord
mkRecordFromStandardCSV [d,o,p,a,de,t]
  = setDate d mkBudgetRecord >>= setOwners o >>=
    setPurchaser p >>= setAmount a >>=
    setDescription de >>= setTags t
mkRecordFromStandardCSV l = Left (ParseError ("Wrong number of fields: " ++ (show l)))

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
  where amt = reads (map replacecommas x) :: [(Double,String)]
        replacecommas ',' = '.'
        replacecommas c = c

setDescription :: String -> BudgetRecord -> Either ParseError BudgetRecord
setDescription d br = Right (br { brDescription = d })

setTags :: String -> BudgetRecord -> Either ParseError BudgetRecord
setTags tagstring br = Right (br { brTags = tgs })
  where tgs = [ trim t | t <- splitOn "," tagstring, t /= "" ]

-- Utility
trim :: String -> String
trim t = ((dropWhile isSpace) . (dropWhileEnd isSpace)) t
