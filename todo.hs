import           Control.Applicative
import           Data.Time.Clock
import           Data.Time.Format


parseDate :: String -> String -> Maybe UTCTime
parseDate = parseTimeM True defaultTimeLocale


parseDateHeading :: String -> Maybe UTCTime
parseDateHeading input =
    parseDate "=== %b %a %d ===" input <|> parseDate "=== %a %d ===" input
