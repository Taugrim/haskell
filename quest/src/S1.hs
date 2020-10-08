module S1 where

--import Data.Time.Clock
--import Data.Time.Format as F
import Data.Time.Clock (UTCTime)
import Data.Time.Format (formatTime, defaultTimeLocale)
import Data.Function ((&))

timeToString :: UTCTime -> String

timeToString = formatTime defaultTimeLocale "%a %d %T"

data LogLevel = Error | Warning | Info
ct = read "2019-02-24 18:28:52.607875 UTC"::UTCTime
data LogEntry = LogEntry {timestamp::UTCTime,logLevel::LogLevel,message::String}

logLevelToString :: LogLevel -> String
logLevelToString Error= "Error"
logLevelToString Warning= "Warning"
logLevelToString Info= "Info"

logEntryToString :: LogEntry -> String
logEntryToString l = (timeToString $ timestamp l)++": "++(logLevelToString $ logLevel l)++": "++  message l


data Person = Person { firstName :: String, lastName :: String, age :: Int } deriving Show

updateLastName :: Person -> Person -> Person
updateLastName p pp=pp {lastName = (  lastName p) }

abbrFirstName :: Person -> Person
abbrFirstName p@Person { firstName =(x:xx:xs)} = p {firstName=(x:".")}
abbrFirstName p = p

data Coord a = Coord a a deriving Show

distance :: Coord Double -> Coord Double -> Double
distance (Coord x y) (Coord x2 y2)=  sqrt ((x2-x) ^ 2 + (y2-y) ^ 2)

manhDistance :: Coord Int -> Coord Int -> Int
manhDistance (Coord x y) (Coord x2 y2)= abs(x2-x)+abs(y2-y)


getCenter :: Double -> Coord Int -> Coord Double
getCenter s (Coord x y)=Coord (c x)(c y) where c cc=((fromIntegral cc)* s+(s/2))

  