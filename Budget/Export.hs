module Budget.Export where
import Budget;
import Budget.Debt;

list'tags :: [Tag] -> Budget -> String
list'tags ts b = unlines $ [ t ++ " : " ++ (sum' t) | t <- ts ]
  where sum' t = show $ Budget.sum $ tagged t b

report :: Person -> Budget -> String
report p b =  "Revenues:\n\n" ++ revS ++
              "\n\nExpenses:\n\n" ++ expS ++
              "\n\nDebts:\n\n" ++ dbtS ++ "\n"
  where
    b'  = purchased p b
    rev = revenues b'
    ex = expenses b'
    dbt = Budget.Debt.debt b
    revS = list'tags (tags rev) rev
    expS = list'tags (tags ex) ex
    dbtS = Budget.Debt.report dbt
