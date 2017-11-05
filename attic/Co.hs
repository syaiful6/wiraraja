
import Control.Monad (forever)
import Control.Monad.Trans.Class (lift)
import Control.Concurrent hiding (yield)
import Web.Wiraraja.DSL.Coroutine

nats :: Producer Int IO ()
nats = go 0
  where
    go i = do
        yield i
        lift $ threadDelay 10000
        go (i + 1)

printer :: Show a => Consumer a IO ()
printer = forever $ do
    s <- await
    lift (putStrLn $ show s)
    pure Nothing

main = runProcess (nats /\ nats $$ printer)
