module Main where
  
import Prelude
import Control.Monad.Eff
-- import Control.Monad.Eff.Console
import DOM -- needed for type signatures
import Data.Either 
import Data.Maybe
import Html as H
import Data.Date hiding (fromString) -- https://github.com/purescript/purescript-datetime
import Data.Traversable -- traverse
import Control.Apply -- <$>
import Data.Foldable -- traverse_?
import Data.Array (range,  head, length, filter)
import Data.String (trim)
import Data.Foreign.Class (read)
import Data.Enum (enumFromTo)
import Data.Int (fromString)
-- import Data.Tuple
--import qualified Halogen.HTML.Properties.Indexed as P
import Control.Monad.Eff.JQuery as J

-- seqname is sometimes header'd as Strain
-- may include more granular location?
-- does disease exist or is it equal to strain?
type RichEntry = { name     :: String
                 , acc      :: String
                 , year     :: Year
                 , month    :: Maybe Month
                 , day      :: Maybe Int
                 , country  :: String
                 , host     :: Host
                 , serotype :: Serotype
                 , sequence :: String
                 , segment  :: Maybe Segment}
                 
type Query = {    name      :: Maybe String
                 , acc      :: Maybe String
                 , minYear  :: Year
                 , maxYear  :: Year
                 , minMonth :: Month
                 , maxMonth :: Month
                 , minDay   :: Int
                 , maxDay   :: Int
                 , country  :: Maybe String
                 , host     :: Maybe Host -- there needs to be a way to turn this dropdown off
                 , serotype :: Maybe Serotype 
                 , segment  :: Maybe Segment}
             
-- serotypes also act as collection names
data Host = Human | Mosquito 
instance showHost :: Show Host where
  show Human = "Human"
  show Mosquito = "Mosquito"
instance eqHost :: Eq Host where
  eq Human Human = true
  eq Mosquito Mosquito = true
  eq _ _ = false
  --neq x y =   not x == y

data Serotype = DENV1 | DENV2 | DENV3 | DENV4
instance showSerotype :: Show Serotype where
  show DENV1 = "DENV1"
  show DENV2 = "DENV2"
  show DENV3 = "DENV3"
  show DENV4 = "DENV4"
instance eqSerotype :: Eq Serotype where -- using eq (show x) (show y) failed
  eq DENV1 DENV1 = true
  eq DENV2 DENV2 = true
  eq DENV3 DENV3 = true
  eq DENV4 DENV4 = true
  eq _ _ = false
  
data Segment = PB1 | PB2 -- ... etc.
instance showSegment :: Show Segment where
  show PB1 = "PB1"
  show PB2 = "PB2"
instance eqSegment :: Eq Segment where
  eq x y = (show x) == (show y)

-- all these Arrays should go back to being Traversables
-- this function would be safer if it accepted only Bounded instances.
rangeOptions :: forall t57. Array String -> Eff ( dom :: DOM | t57) (Array J.JQuery) 
rangeOptions = traverse makeOption
  where makeOption x = withAttr "<option>" "value" x >>= (J.setText x) >>= return
          
-- forall t10 t11. String -> String -> t11 -> Eff ( dom :: DOM | t10) JQuery
withAttr h f v = J.create h >>= (J.setAttr f v) >>= return

-- forall t18 t19. String -> t18 -> Eff ( dom :: DOM | t19) JQuery
withId h id = withAttr h "id" id

appendAll  ::    forall t171 t178. (Traversable t171) => J.JQuery -> t171 J.JQuery -> Eff ( dom :: DOM | t178) Unit --(t171 JQuery) 
appendAll x xs = traverse_ (flip J.append x) xs

rangeDropdown id xs = do
   select <- withId "<select>" id
   options <- rangeOptions xs
   appendAll select options
   return select
--main = J.ready $ do
--  H.ul_
--   [H.makeElem_ "<li>"
--     [H.makeElem "<p>" (H.Attr "text" "foobar") []]]
                 
makeTextInput id = do
  --input <- withId id "<input>"
  input <- J.create "<input>"
  p <- J.create "<b>"
  J.setText (id ++ " ") p
  J.setAttr "id" id input
  J.setAttr "value" ""    input
  J.setAttr "type" "text" p
  J.append input p
  return p
  
--                 , host     :: Maybe Host
--                 , serotype :: Maybe Serotype 
main = do
  body <- J.body
  --textInputs <- traverse makeTextInput ["name", "acc", "country"]
  name <- makeTextInput "name"
  acc <- makeTextInput "acc"
  country <- makeTextInput "country"
  minmonthSelect <-  rangeDropdown "minmonth" $ map show $ enumFromTo January December
  minyearSelect <-   rangeDropdown "minyear"  $ map show $ range 1900 2016
  mindaySelect <-    rangeDropdown "minday"   $ map show $ range 1 31
  maxmonthSelect <-  rangeDropdown "maxmonth" $ map show $ enumFromTo January December
  maxyearSelect <-   rangeDropdown "maxyear"  $ map show $ range 1900 2016
  maxdaySelect <-    rangeDropdown "maxday"   $ map show $ range 1 31
  serotypeSelect <- rangeDropdown  "serotype" $ map show $ serotypes
  segmentSelect <- rangeDropdown  "segment" $ map show $ segments
  -- match the string of  the result with the Enum or else use .selectedIndex
   --Right v <- (read <$> (J.getProp "selectedIndex" serotypeSelect))
  --J.setText v name
  appendAll body ([name, acc, country] ++ [minmonthSelect, serotypeSelect, segmentSelect])
    ---indexOf x xs = takewhile (x /=) xs
years = range 1900 2016
days = range 1 31
months = enumFromTo January December
serotypes = [DENV1, DENV2, DENV3, DENV4]
segments = [PB1, PB2]
hosts = [Mosquito, Human]
main' = J.ready $ do
  body <- J.body
  select <- rangeDropdown "month" (map show $ enumFromTo January December)
  btn  <- J.create "<button>"
  text  <- J.create "<p>" 
  J.setText "unclicked" text
  traverse_ (flip J.append body) [text, select, btn] -- was traverse (flip ...
--  J.on "click" (handleClick select text) btn
--  where
--    handleClick input text _ _  = do
--      v <- selectId "month"
--      J.setText v text
-- validating query must check that at least one field is not nothing.
  
-- NOTE: Some way to use Last in Data.Maybe
-- NOTE: just wrap left in Just: Just(x.acc) == q.acc
match :: Query -> RichEntry -> Boolean
match q x = 
 (q.acc ==? x.acc) && (q.name ==? x.name) && (q.serotype ==? x.serotype) && datesMatch
 where
   -- the below type declartion is necessary, otherwise it gets infered as String -> String -> Boolean I guess.
   (==?) :: forall a. (Eq a) => Maybe a -> a -> Boolean -- note there is only one type variable!
   (==?) a b = fromMaybe true ((== b) <$> a)
   datesMatch = yearInRange && monthInRange && dayInRange
     where
       yearInRange = (between q.minYear q.maxYear x.year)
       monthInRange = (fromMaybe true $ (between q.minMonth q.maxMonth) <$> x.month)
       dayInRange = (fromMaybe true $ (between q.minDay q.maxDay) <$> x.day)
       -- same here. it seems to get inferred simply from the first call.
       between :: forall a. (Ord a) => a -> a -> a -> Boolean
       between min max x =  (min <= x && x <= max)
      

  -- do validation and convert fields to types and put all together into a record(?)
  -- TODO: switch <select> to store an int value, and use with
  -- Data.Enum.toEnum, e.g.
    -- toEnum 0 :: Maybe Month == January
    -- (that validates all the enum fields) 
-- equivalent to handleClick (and will replace it)
handleQuery input text _ _  = do
  host     <- toEnum hosts <$> (selectId "host")
  serotype <- toEnum serotypes <$> (selectId "serotype")
  segment  <- toEnum segments <$> (selectId "segment")
  month    <- toEnum months <$> (selectId "month")
  --year   <- (selectId "year") >>= (\x -> return $ Year <$> (fromString x))
  yearInt  <- fromString <$> (selectId "year")
  year     <- return $ Year <$> yearInt
  day      <- fromString <$> (selectId "month")
  acc      <- fetch id "acc"
  disease  <- fetch id "disease"
  country  <- fetch id "country"
  return country -- TODO: convert to Query and run query
  where 
    fetch f id = f <$> toMaybe <$> selectId id 
    toMaybe s = if ((trim s) == "") then Nothing else Just s
    toEnum :: forall t. (Show t) => Array t -> String -> Maybe t
    toEnum xs s = head $ filter (\x -> s == (show x)) xs
    --note: strings are not character arrays in purescript

selectId ::  forall t5. String -> Eff ( dom :: DOM | t5) String 
selectId s = do
   res <- J.select ("#" ++ s)
   --TODO: I don't understand this bit with read.
   Right v <- read <$> J.getValue res
   return v

--   No type class instance was found for . .. can often be cured by providing type signature 


