module Calc where
    import Text.Megaparsec
    import Text.Megaparsec.Char
    import Data.Void

    data Law = Law LawName Equation
        deriving (Eq)
    type LawName = String
    type Equation = (Expr, Expr)
    type Parser = Parsec Void String 
    type Subst = [(Expr, Expr)]

    data Step = Step LawName Expr deriving Eq
    data Calculation = Calculation Expr [Step]

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
    data Expr = TwoOp BOp Expr Expr
          | OneOp UOp Expr
          | Derive Expr Expr
          | Var Char
          | Const Char 
          deriving (Eq)

    -- show instances https://stackoverflow.com/questions/12537120/making-a-data-type-an-instance-of-show-in-haskell
    instance Show BOp where
        show Add = " + "
        show Mult = " * "
        show Div = " / "
        show Subt = " - "
        show Power = "^"

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

    instance Show Step where
        show (Step name exp) = "= {" ++ (show name) ++ "}\n" ++ (show exp) ++ "\n"