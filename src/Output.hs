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

    --list of steps
    manyStep :: [Law] -> Expr -> [Step]
    manyStep ls e = case steps of
                        [] -> []
                        (Step name exp):_ -> (Step name exp) : (manyStep ls exp)
                    where steps = [Step name res | Law name (e1, e2) <- ls, res <- (rewrites e1 e2 e)]

    --creates a calculation
    derive :: [Law] -> Expr -> Calculation
    derive ls e = Calculation e (manyStep ls e)
        