module Budget where
import           Data.List
import           Data.Time.Calendar
import           Data.Time.LocalTime
import           Text.Printf         (printf)

-- Data definitions
data BudgetRecord = BudgetRecord {
    brDate        :: LocalTime
  , brOwners      :: [Person]
  , brPurchaser   :: Person
  , brAmount      :: Double
  , brDescription :: String
  , brTags        :: [Tag]
  } deriving (Show,Eq)

type Budget = [BudgetRecord]
type Tag = String
type Person = String

mkBudgetRecord :: BudgetRecord
mkBudgetRecord = BudgetRecord (LocalTime (ModifiedJulianDay 0) midnight) [] "" 0 "" []

-- Compile information about Budget

-- | List all tags contained in Budget
tags :: Budget -> [Tag]
tags b = sort $ Data.List.nub $ foldl f [] b
  where f acc x = acc ++ (brTags x)

-- | Sum of all amounts in Budget
sum :: Budget -> Double
sum = foldl f 0
  where f acc x = acc + brAmount x

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
