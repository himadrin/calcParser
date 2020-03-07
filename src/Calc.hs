module Calc where
    import Prelude hiding (exp)
    import Text.Megaparsec
    import Data.Void

    data Law = Law LawName Equation
        deriving (Eq, Show)
    
    type Laws = [Law]
    type LawName = String

    --comma seperates left and right side of equation
    type Equation = (Expr, Expr)
    type Parser = Parsec Void String 
    type Subst = [(Expr, Expr)]

    --needed for parse
    data Err e = Expr e 
            | Error String deriving (Eq)

    data Step = Step LawName Expr deriving Eq

    --defines operators
    data BOp = Add 
          | Mult
          | Div 
          | Subt
          | Power
          deriving (Eq)
    data UOp = Sin 
          | Cos
          | Tan
          | Ln 
          | Neg 
          | Pos 
          deriving (Eq)

    --puts expressions together with type and operator
    data Expr = TwoOp BOp Expr Expr
          | OneOp UOp Expr
          | Derive Expr Expr
          | Var Char
          | Const Int 
          deriving (Eq)

    -- show instances https://stackoverflow.com/questions/12537120/making-a-data-type-an-instance-of-show-in-haskell
    instance Show BOp where
        show Power = "^"
        show Add = " + "
        show Subt = " - "
        show Mult = " * "
        show Div = " / "

    instance Show UOp where
        show Sin = "sin "
        show Cos = "cos "
        show Tan = "tan "
        show Ln = "ln "
        show Pos = "+"
        show Neg = "-"

    instance Show Expr where
        show (TwoOp two exp1 exp2) = "(" ++ (show exp1) ++ (show two) ++ (show exp2) ++ ")"
        show (OneOp one exp) = (show one) ++ "(" ++ (show exp) ++ ")"
        show (Derive var exp) = "(derive " ++ (show var) ++ ") " ++ (show exp)
        show (Var c) = [c]
        show (Const i) = show i

    --used formatting from pretty printing proofs
    instance Show Step where
        show (Step name exp) = "= {" ++ (show name) ++ "}\n" ++ (show exp) ++ "\n"

    -- this shows the correct expr or the error message
    instance Show e => Show (Err e) where
        show (Expr e) = show e
        show (Error e) = e