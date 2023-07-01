type Info = String

data Term =
    TmTrue Info
    | TmFalse Info
    | TmIf Info Term Term Term
    | TmZero Info
    | TmSucc Info Term
    | TmPred Info Term
    | TmIsZero Info Term

isNumericVal :: Term -> Bool
isNumericVal (TmZero _) = True
isNumericVal (TmSucc _ t1) = isNumericVal t1
isNumericVal _ = False

main = do
    print $ isNumericVal (TmSucc "info" (TmZero "info"))
    print $ isNumericVal (TmFalse "info")