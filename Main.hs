import Lambda
import Combinator

main :: IO ()
main = do
  let f = (Lam "x" (Lam "y" z))
  let s = (Lam "y" (Lam "x" y))
  print $ alphaEq f s
  print $ alphaEq s f
