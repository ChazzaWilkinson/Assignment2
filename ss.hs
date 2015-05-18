{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
import Control.Monad    (msum)
import Happstack.Server
    ( Response, ServerPart, Method(POST)
    , BodyPolicy(..), decodeBody, defaultBodyPolicy
    , dir, look, nullConf, ok, simpleHTTP
    , toResponse, methodM
    )
import Text.Blaze                         as B
import Text.Blaze.Html4.Strict            as B hiding (map)
import Text.Blaze.Html4.Strict.Attributes as B hiding ( dir, label
                                                      , title)

import System.IO.Unsafe (unsafePerformIO)
import Control.Applicative
import Database.SQLite.Simple         
import Database.SQLite.Simple.FromRow 
import Database.SQLite.Simple.FromField as Char
import GHC.Generics
--import Data.Aeson
--import qualified Data.ByteString.Lazy as C




data Temperature = Temperature {
    date :: String,
    temp :: Int
  }deriving(Show, Generic)

data TemperatureAv = TemperatureAv {
    tempAvr :: Float
  }deriving(Show, Generic)

data TemperatureLow = TemperatureLow {
    tempLow :: Integer
  }deriving(Show, Generic)

data TemperatureHigh = TemperatureHigh {
    tempHigh :: Integer
  }deriving(Show, Generic)

main :: IO ()
main = simpleHTTP nullConf $ handlers 

instance FromRow Temperature where
	fromRow = Temperature <$> field <*> field

instance FromRow TemperatureAv where
	fromRow = TemperatureAv <$> field

instance FromRow TemperatureLow where
	fromRow = TemperatureLow <$> field

instance FromRow TemperatureHigh where
	fromRow = TemperatureHigh <$> field

--instance FromJSON Temperature
--instance ToJSON Temperature


--jsonURL :: String
--jsonURL = "http://www.phoric.eu/temperature.json"

--etJSON :: IO C.ByteString
--getJSON = simpleHttp jsonURL


queryDatabaseAvr :: IO[(TemperatureAv)]
queryDatabaseAvr = do
		conn <- open "test.db"
		r <- query_ conn "SELECT AVG(Temp) FROM table1" :: IO[(TemperatureAv)]
		close conn
		return r

queryDatabaseLow :: IO[(TemperatureLow)]
queryDatabaseLow = do
		conn <- open "test.db"
		r <- query_ conn "SELECT MIN(Temp) FROM table1" :: IO[(TemperatureLow)]
		close conn
		return r

queryDatabaseHigh :: IO[(TemperatureHigh)]
queryDatabaseHigh = do
		conn <- open "test.db"
		r <- query_ conn "SELECT MAX(Temp) FROM table1" :: IO[(TemperatureHigh)]
		close conn
		return r

queryDatabaseAll :: IO[(Temperature)]
queryDatabaseAll = do
		conn <- open "test.db"
		r <- query_ conn "SELECT * FROM table1" :: IO[(Temperature)]
		close conn
		return r

--queryDatabaseTemp :: String -> IO[(Temperature)]
--queryDatabaseTemp temp = do
		--conn <- open "test.db"
		--r <- query conn "SELECT * FROM table1 WHERE Temp = ?"  [temp] :: IO[(Temperature)]
		--close conn
		--return r


tempPick :: Temperature -> Int
tempPick (Temperature _ t) = t

datePick :: Temperature -> String
datePick (Temperature d _) = d

tempToStringList :: [Temperature] -> [String]
tempToStringList (x:xs) = ( (datePick x) ++ " - " ++ show(tempPick x)) : tempToStringList xs
tempToStringList [] = [] 

tempToString :: [String] -> String
tempToString (x:xs) = x ++ ", " ++ tempToString xs
tempToString [] = "" 
 
avrToStringList :: [TemperatureAv] -> [String]
avrToStringList (x:xs) = (show(x)) : avrToStringList xs
avrToStringList [] = [] 

lowToStringList :: [TemperatureLow] -> [String]
lowToStringList (x:xs) = (show(x)) : lowToStringList xs
lowToStringList [] = []

highToStringList :: [TemperatureHigh] -> [String]
highToStringList (x:xs) = (show(x)) : highToStringList xs
highToStringList [] = []

myPolicy :: BodyPolicy
myPolicy = (defaultBodyPolicy "/tmp/" 0 1000 1000)

handlers :: ServerPart Response
handlers =
    do decodeBody myPolicy
       msum [ dir "all" $ allForm 
	    --, dir "temp" $ tempForm 
	    , dir "avr" $ avrForm
	    , dir "low" $ lowForm
	    , dir "high" $ highForm
            , mainForm
            ]


mainForm :: ServerPart Response
mainForm = ok $ toResponse $
    html $ do
      B.head $ do
	  title "Temp Data"
          h1 "Temperature Data"
      B.body $ do

	      form ! enctype "multipart/form-data"
	           ! action "json" $ do
                      label "JSON Data"
	              input ! type_ "submit"
                            ! name "SubmitJSONQuery"

	      
	      h2 "Query More Data: "

	      br
	      

              form ! enctype "multipart/form-data"
	           ! action "date" $ do
                      label "Enter Date: "
		      input ! type_ "date"
			    ! name "tempQueryText"
			    ! size "5"
	              input ! type_ "submit"
                            ! name "SubmitTempQuery"


	      form ! enctype "multipart/form-data"
	           ! action "temp" $ do
                      label "Enter Temperature: "
		      input ! type_ "text"
			    ! name "tempQueryText"
			    ! size "5"
	              input ! type_ "submit"
                            ! name "SubmitTempQuery"

      
	      form ! enctype "multipart/form-data"
	           ! action "avr" $ do
                      label "Average Temperature: "
	              input ! type_ "submit"
                            ! name "SubmitAvrQuery"


	      form ! enctype "multipart/form-data"
	           ! action "all" $ do
                      label "Select All: "
	              input ! type_ "submit"
                            ! name "Submit Query"


	      form ! enctype "multipart/form-data"
	           ! action "high" $ do
                      label "Highest Temperature: "
	              input ! type_ "submit"
                            ! name "SubmitHighQuery"


	      form ! enctype "multipart/form-data"
	           ! action "low" $ do
                      label "Lowest Temperature: "
	              input ! type_ "submit"
                            ! name "SubmitLowQuery"
			


allForm :: ServerPart Response
allForm = ok $ toResponse $
    html $ do 
      B.head $ do
	  title "All Data"
          h1 "All Data"
      B.body $ do
	  p $ toHtml $ tempToString $ tempToStringList $ unsafePerformIO $ queryDatabaseAll


--tempForm :: ServerPart Response
--tempForm = ok $ toResponse $
    --html $ do 
     -- B.head $ do
	 -- title "Specific Temp Data"
         -- h1 "Specific Temp Data"
      --B.body $ do
	  --p $ toHtml $ tempToString $ tempToStringList $ unsafePerformIO $ queryDatabaseTemp


avrForm :: ServerPart Response
avrForm = ok $ toResponse $
    html $ do 
      B.head $ do
	  title "Average Temp"
          h1 "Average Temperature"
      B.body $ do
	  p $ toHtml $ tempToString $ avrToStringList $ unsafePerformIO $ queryDatabaseAvr

lowForm :: ServerPart Response
lowForm = ok $ toResponse $
    html $ do 
      B.head $ do
	  title "Average Temp"
          h1 "Average Temperature"
      B.body $ do
	  p $ toHtml $ tempToString $ lowToStringList $ unsafePerformIO $ queryDatabaseLow

highForm :: ServerPart Response
highForm = ok $ toResponse $
    html $ do 
      B.head $ do
	  title "Average Temp"
          h1 "Average Temperature"
      B.body $ do
	  p $ toHtml $ tempToString $ highToStringList $ unsafePerformIO $ queryDatabaseHigh


