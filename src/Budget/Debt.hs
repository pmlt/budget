module Budget.Debt where
import Budget (Person,BudgetRecord,Budget,brOwners,brPurchaser,brAmount)
import Data.List (nub)

data DebtRecord = DebtRecord {
    drCreditor :: Person
  , drDebtor :: Person
  , drAmount :: Double
  } deriving (Show,Eq)

type Debt = [DebtRecord]

-- | Analyze debt from budget report
debt :: Budget -> Debt
debt = debt' []

-- | Accumulator version of debt
debt' :: Debt -> Budget -> Debt
debt' d [] = d
debt' d (x:xs)
  | length o == 1 && (head o) == p = debt' d xs --No debt here
  | otherwise = debt' d' xs
  where
    o = brOwners x
    ol = fromIntegral (length o)
    p = brPurchaser x
    a = brAmount x
    o' = filter (/= p) o -- Debtors
    d' = d ++ [ DebtRecord p dtor (a / ol) | dtor <- o' ]

-- | Sum records of debt with same creditor/debtor into single record
compress :: Debt -> Debt
compress dl = [ DebtRecord c d (sum' c d) | c <- creds, d <- dbts, c /= d ]
  where
    creds = nub [ drCreditor x | x <- dl ]
    dbts = nub [ drDebtor x | x <- dl ]
    sum' c d = Budget.Debt.sum [ dr | dr <- dl, c == drCreditor dr, d == drDebtor dr ]

-- | Sum debt records
sum :: Debt -> Double
sum d = foldr ((+).drAmount) 0.0 d

-- | Extract debts with specific debtor
creditors :: Person -> Debt -> Debt
creditors p d = [ x | x <- d, p == drDebtor x ]

-- | Extract debts with specific creditor
debtors :: Person -> Debt -> Debt
debtors p d = [ x | x <- d, p == drCreditor x ]

-- | Extract all people involved in debts
people :: Debt -> [Person]
people dl = nub (creds ++ dbts)
  where
    creds = nub [ drCreditor x | x <- dl ]
    dbts = nub [ drDebtor x | x <- dl ]

-- | Sum amount owed by one person to another
owed :: Person -> Person -> Debt -> DebtRecord
owed d c dbt = foldl f (DebtRecord c d 0) dbt
  where
    f t (DebtRecord c' d' a)
      | c == c' && d == d' = t { drAmount = tot + a }
      | c == d' && d == c' = t { drAmount = tot - a } --Inverse
      | otherwise = t
      where tot = drAmount t
