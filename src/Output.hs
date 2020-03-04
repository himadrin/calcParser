module Output where

    -- import Parser as P
    import Calc
    import Implementation
    import Text.Megaparsec
    import Text.Megaparsec.Char
    import Data.Void
    import Control.Monad.Combinators.Expr
    import qualified Text.Megaparsec.Char.Lexer as L
    import System.IO()
    import Prelude hiding (exp)

    --one step
    makeStep :: [Law] -> Expr -> [Step]
    makeStep law_list expression = [Step name res | Law name (e1, e2) <- law_list, res <- (rewrites e1 e2 expression)]

    --list of steps
    manyStep :: [Law] -> Expr -> [Step]
    manyStep law_list expression = case steps of
                        [] -> []
                        (Step name exp):_ -> (Step name exp) : (manyStep law_list expression)
                        where steps = makeStep law_list expression

    --creates a calculation
    derive :: [Law] -> Expr -> Calculation
    derive law_list expression = Calculation e (manyStep law_list expression)
