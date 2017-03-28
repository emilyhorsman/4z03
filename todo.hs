import           Control.Applicative
import           Control.Concurrent
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


main :: IO ()
main =
    forever $ getLine >>= forkIO . handleInput
