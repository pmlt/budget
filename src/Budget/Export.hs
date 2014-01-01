module Budget.Export where
import Budget;
import Budget.Debt;
import Text.Printf (printf);
import Data.List;

list'tags :: [Tag] -> Budget -> String
list'tags ts b = unlines $ [ report'line t (sum'tags t) | t <- ts ]
  where sum'tags t = Budget.sum $ tagged t b

report'line :: String -> Double -> String
report'line t a = printf "%30s : %s" t (fmt a)

debt'line :: DebtRecord -> String
debt'line (DebtRecord c d a) = printf "%30s : %s" (d ++ " -> " ++ c) (Budget.fmt a)

report :: Person -> Budget -> String
report p b =  "Revenues:\n\n" ++ revS ++
              "\n\nExpenses:\n\n" ++ expS ++
              "\n\nDebts:\n\n" ++ dbtS ++
              "\n\nTotals:\n\n" ++ 
                (report'line "Total Revenues" revTotal) ++ "\n" ++
                (report'line "Total Expenses" expTotal) ++ "\n" ++
                (report'line "Total Debts" debtTotal) ++ "\n" ++
                (report'line "Total" supercalifragilisticexpialidociousTotal) ++ "\n"
  where
    b'  = purchased p b
    rev = revenues b'
    revS = list'tags (tags rev) rev
    revTotal = Budget.sum rev
    ex = expenses b'
    expS = list'tags (tags ex) ex
    expTotal = Budget.sum ex
    dbt = Budget.Debt.debt b
    dbtPeople = delete p $ people dbt
    personalDebts = map f dbtPeople
    f x = owed p x dbt
    dbtS = unlines $ map debt'line personalDebts
    debtTotal = (Budget.Debt.sum personalDebts)
    supercalifragilisticexpialidociousTotal = revTotal + expTotal + debtTotal
