import           Control.Applicative
import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Monad
import           Data.Time.Clock
import           Data.Time.Format


parseDate :: String -> String -> Maybe UTCTime
parseDate = parseTimeM True defaultTimeLocale


parseDateHeading :: String -> Maybe UTCTime
parseDateHeading input =
    parseDate "=== %b %a %d ===" input <|> parseDate "=== %a %d ===" input


handleInput :: String -> IO ()
handleInput l =
    threadDelay (truncate 3e6) >> putStrLn l


increment :: Num a => TVar a -> STM a
increment num =
    readTVar num >>= (\n -> writeTVar num (n + 1) >> return (n + 1))


main :: IO ()
main =
    let
        n = newTVar 0
    in
        (atomically $ (n >>= increment)) >>= (putStrLn . show)
    --forever $ getLine >>= forkIO . handleInput
