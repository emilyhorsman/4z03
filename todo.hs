{-# LANGUAGE OverloadedStrings #-}
import           Control.Applicative
import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Monad
import           Data.ByteString.Builder
import qualified Data.ByteString.Lazy     as LazyBS
import           Data.Time.Clock
import           Data.Time.Format
import           Network.HTTP.Types
import           Network.Wai
import           Network.Wai.Handler.Warp (run)
import           Text.Printf


parseDate :: String -> String -> Maybe UTCTime
parseDate = parseTimeM True defaultTimeLocale


parseDateHeading :: String -> Maybe UTCTime
parseDateHeading input =
    parseDate "=== %b %a %d ===" input <|> parseDate "=== %a %d ===" input


handleInput :: TVar Int -> String -> IO ()
handleInput n l = do
    threadDelay (truncate 1e6)
    putStrLn l
    count <- atomically $ increment n
    putStrLn $ printf "Incremented %d times!" count


increment :: TVar Int -> STM Int
increment num =
    readTVar num >>= writeTVar num . (+1) >> readTVar num


intToLBS :: Int -> LazyBS.ByteString
intToLBS = toLazyByteString . intDec


cmdEcho :: IO ()
cmdEcho = do
    n <- atomically $ newTVar (0 :: Int)
    forever $ getLine >>= forkIO . handleInput n


app :: TVar Int -> Application
app counter _ respond =
    threadDelay (truncate 1e6)
    >> (atomically $ increment counter)
    >>= (\n -> respond $ responseLBS status200 [] (intToLBS n))


main :: IO ()
main = do
    n <- atomically $ newTVar (0 :: Int)
    run 8080 (app n)
