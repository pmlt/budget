module Main where
import System.Environment (getArgs);
import Budget;
import Budget.Export;
import System.Console.GetOpt;
import Text.CSV (CSV,parseCSVFromFile);

data Flag = Version | Person String deriving (Eq)

header :: String
header = "Usage: Budget [OPTION...] file.csv"

options :: [OptDescr Flag]
options = 
  [ Option ['v','?']  ["version"] (NoArg Version) "show version number"
  , Option ['p']      ["person"]  (ReqArg Person "PERSON") "subject person"
  ]

parseOpts :: [String] -> IO ([Flag], String)
parseOpts argv = 
  case getOpt Permute options argv of
    (o,[f],[]) -> return (o,f) -- Only ONE arg allowed
    (_,_,errs) -> ioError (userError (concat errs ++ usageInfo header options))

parseCSV :: String -> IO CSV
parseCSV f = do
  parseResult <- parseCSVFromFile f
  case parseResult of
    Left err -> ioError $ userError $ "Could not parse CSV file: " ++ (show err)
    Right csv  -> return csv

parseBudget :: CSV -> IO Budget
parseBudget csv = case mkBudgetFromCSV csv of
  Left errors  -> ioError $ userError $ "CSV file is not a valid Budget: \n" ++ (unlines $ map show errors)
  Right budget -> return budget

main :: IO ()
main = do
  argv <- getArgs
  (opts,file) <- parseOpts argv
  if (Version `elem` opts) then do
      putStrLn "Budget: v. 0.1"
    else do
      Person person <- return $ head opts
      csv <- parseCSV file
      budget <- parseBudget csv
      putStrLn $ report person budget
