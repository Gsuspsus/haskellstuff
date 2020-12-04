import Control.Monad
import Text.Read
import Control.Arrow (left)

type Token = String
type Operator = String

data Expression = Expression String Double Operator deriving (Show, Read)

main :: IO ()
main = do
    --check if command line args
    -- if so parse them into [String]
    -- else read line by line from input
    input <- getLine
    when (null input) $ return ()


tokenizeExpr :: String -> Either String [Token] 
tokenizeExpr s = if length expr /= 3 then Left "Invalid expression"
               else Right expr
    where 
        expr = words s

parseTokens :: [Token] -> Either String Expression
parseTokens toks = do 
    let description = (readEither $ show (toks !! 0)) :: Either String String
    let number = (readEither (toks !! 1)) :: Either String Double
    let op = (readEither $ show (toks !! 2)) :: Either String String
    Expression <$> description <*> number <*> op
