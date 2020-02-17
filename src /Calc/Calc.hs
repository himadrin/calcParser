module Calc.Calc where
{-
    newtype Exp = Compose [Atom] derivingEq 
    data Atom = Var String | Con String [Exp] derivingEq
    
    operator:: Parser String
    atom:: Parser Atom
    -}