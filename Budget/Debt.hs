module Budget.Debt where
import Budget (Person,BudgetRecord,Budget,brOwners,brPurchaser,brAmount)

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
compress dl = [ DebtRecord c d (sum' c d) | c <- creds, d <- dbts ]
  where
    creds = [ drCreditor x | x <- dl ]
    dbts = [ drDebtor x | x <- dl ]
    sum' c d = foldr ((+).drAmount) 0.0 [ dr | dr <- dl, c == drCreditor dr, d == drDebtor dr ]

-- | Extract debts with specific debtor
creditors :: Person -> Debt -> Debt
creditors p d = [ x | x <- d, p == drDebtor x ]

-- | Extract debts with specific creditor
debtors :: Person -> Debt -> Debt
debtors p d = [ x | x <- d, p == drCreditor x ]

-- | Sum amount owed by one person to another
owed :: Person -> Person -> Debt -> Double
owed d c dbt = foldl f 0 dbt
  where
    f t (DebtRecord c' d' a)
      | c == c' && d == d' = t + a
      | otherwise = t

-- | Output report
report :: Debt -> String
report dbt = unlines [ f dr | dr <- subDbt ]
  where
    f (DebtRecord c d a) = d ++ " owes " ++ (show a) ++ "$ to " ++ c
    subDbt = compress dbt