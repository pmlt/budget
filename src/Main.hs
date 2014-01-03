module Main where
import System.Environment (getArgs);
import Budget;
import Budget.Export;
import Budget.Import;
import System.Console.GetOpt;
import Data.List

data Flag = Version
          | Person String
          | DesjardinsFile String
          | VisaFile String
          | StandardFile String
          deriving (Eq,Show)

header :: String
header = "Usage: Budget [OPTION...] -p P -d desjardins.csv -i visa.csv -f other.csv"

options :: [OptDescr Flag]
options = 
  [ Option ['d']      ["desjardins"] (ReqArg DesjardinsFile "DESJARDINSFILE") "desjardins exported file"
  , Option ['i']      ["visa"] (ReqArg VisaFile "VISAFILE") "VISA exported file"
  , Option ['f']      ["standard"] (ReqArg StandardFile "STANDARDFILE") "Standard-formatted file"
  , Option ['p']      ["person"]  (ReqArg Person "PERSON") "subject person"
  , Option ['v','?']  ["version"] (NoArg Version) "show version number"
  ]

parseOpts :: [String] -> IO [Flag]
parseOpts argv = 
  case getOpt Permute options argv of
    (o,_,[]) -> return o
    (_,_,errs) -> fail $ concat errs ++ usageInfo header options

sortOpts :: [Flag] -> IO (Flag,[Flag])
sortOpts opts = case partition isperson opts of
  ([p],(f:fs)) -> return (p,f:fs)
  ([],_)       -> fail $ "You must provide a single person!\n" ++ (usageInfo header options)
  ((_:_),_)    -> fail $ "You must provide a single person!\n" ++ (usageInfo header options)
  (_,[])       -> fail $ "You must provide at least one input file!\n" ++ (usageInfo header options)
  where isperson (Person _) = True
        isperson _          = False

parseBudget :: Flag -> IO Budget
parseBudget (DesjardinsFile f) = Budget.Import.desjardins f
parseBudget (VisaFile f)       = Budget.Import.visa f
parseBudget (StandardFile f)   = Budget.Import.standard f
parseBudget _                  = fail "Unsupported file type!"

compileBudget :: [Flag] -> IO Budget
compileBudget files = do
  budgets <- mapM parseBudget files
  return $ concat budgets

main :: IO ()
main = do
  argv <- getArgs
  opts <- parseOpts argv
  if (Version `elem` opts) then do
      putStrLn "Budget: v. 1.0"
    else do
      (Person person,files) <- sortOpts opts
      budget <- compileBudget files
      putStrLn $ report person budget