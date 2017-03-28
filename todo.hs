import Control.Applicative
import Data.Time.Format
import Data.Time.Clock

parseDate :: String -> String -> Maybe UTCTime
parseDate = parseTimeM True defaultTimeLocale

parseDateHeading :: String -> Maybe UTCTime
parseDateHeading input =
    parseDate "=== %b %a %d ===" input <|> parseDate "=== %a %d ===" input
