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
    stepList :: [Law] -> Expr -> [Step]
    stepList law_list expression = case steps of
                        [] -> []
                        (Step name exp):_ -> (Step name exp) : (stepList law_list exp)
                    where steps = [Step name new_exp | Law name (e1, e2) <- law_list, new_exp <- (rewrites e1 e2 expression)]
