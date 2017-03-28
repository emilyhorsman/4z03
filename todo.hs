import           Control.Applicative
import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Monad
import           Data.Time.Clock
import           Data.Time.Format
import Text.Printf


parseDate :: String -> String -> Maybe UTCTime
parseDate = parseTimeM True defaultTimeLocale


parseDateHeading :: String -> Maybe UTCTime
parseDateHeading input =
    parseDate "=== %b %a %d ===" input <|> parseDate "=== %a %d ===" input


handleInput :: (Num a, PrintfArg a) => TVar a -> String -> IO ()
handleInput n l = do
    threadDelay (truncate 1e6)
    putStrLn l
    count <- atomically $ increment n
    putStrLn $ printf "Incremented %d times!" count


increment :: Num a => TVar a -> STM a
increment num =
    readTVar num >>= (\n -> writeTVar num (n + 1) >> return (n + 1))


main :: IO ()
main = do
    n <- atomically $ newTVar (0 :: Integer)
    forever $ getLine >>= forkIO . (handleInput n)
