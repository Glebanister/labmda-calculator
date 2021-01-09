import Control.Monad.State
  ( MonadState (get, state),
    MonadTrans (lift),
    State,
    StateT,
    evalStateT,
  )
import Kernel (empmtyScope, routine)

main :: IO ()
main = putStrLn "Welcome to lambda calculator!" >> evalStateT routine empmtyScope
