import Combinator ()
import Lambda
import Text.Parsec
import Typing ()

main :: IO ()
main = do
  print $ runParser parseExpression "" "" "\\x1 x2 x3 -> x1 x2 x2"
  return ()
