module Output where

    -- import Parser as P
    import Calc
    import Implementation
    import Laws
    import Text.Megaparsec
    import Text.Megaparsec.Char
    import Data.Void
    import Control.Monad.Combinators.Expr
    import qualified Text.Megaparsec.Char.Lexer as L
    import System.IO()
    import Prelude hiding (exp)

    --uses rewrites to create list of steps
    stepList :: [Law] -> Expr -> [Step]
    stepList law_list expression = case steps of
                        [] -> []
                        (Step name exp):_ -> (Step name exp) : (stepList law_list exp)

                    -- recursively calls rewrites and doMath on expr for each step
                    where steps = [Step name new_exp | Law name (e1, e2) <- law_list, 
                                                        new_exp <- (map doMath (rewrites e1 e2 expression))]

    --called in main to handle expression and error from parse - takes in laws and expr
    final :: [Law] -> Err Expr -> Err [Step]
    final law_list (Expr e) = Expr (stepList law_list e)
    final _ (Error string) = Error string