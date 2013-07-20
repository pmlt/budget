module Budget.Export where
import Budget;
import Budget.Debt;
import Text.Printf (printf)

list'tags :: [Tag] -> Budget -> String
list'tags ts b = unlines $ [ report'line t (sum'tags t) | t <- ts ]
  where sum'tags t = Budget.sum $ tagged t b

report'line :: String -> Double -> String
report'line t a = printf "%30s : %s" t (fmt a)

report :: Person -> Budget -> String
report p b =  "Revenues:\n\n" ++ revS ++
              "\n" ++ (report'line "Total" revTotal) ++
              "\n\nExpenses:\n\n" ++ expS ++
              "\n" ++ (report'line "Total" expTotal) ++
              "\n\nDebts:\n\n" ++ dbtS ++ "\n"
  where
    b'  = purchased p b
    rev = revenues b'
    ex = expenses b'
    dbt = Budget.Debt.debt b
    revS = list'tags (tags rev) rev
    revTotal = Budget.sum rev
    expS = list'tags (tags ex) ex
    expTotal = Budget.sum ex
    dbtS = Budget.Debt.report p dbt
