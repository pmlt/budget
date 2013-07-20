module Budget.Export where
import Budget;
import Budget.Debt;
import Text.Printf (printf)

list'tags :: [Tag] -> Budget -> String
list'tags ts b = unlines $ [ line t | t <- ts ]
  where sumstr t = fmt $ Budget.sum $ tagged t b
        line t = printf "%30s : %s" t (sumstr t)

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
