module Main where
  
import Prelude
import Control.Monad.Eff
import Control.Monad.Eff.Console
import DOM -- needed for type signatures
import Data.Either 
import Data.Maybe

import Data.Date -- https://github.com/purescript/purescript-datetime
import Data.Traversable -- traverse
import Control.Apply -- <$>
import Data.Foldable -- traverse_?
import Data.Array -- (:)
import Data.String (trim)
import Data.Foreign.Class (read)
import Data.Enum (enumFromTo)
import Data.Tuple
import qualified Halogen.HTML.Properties.Indexed as P
import Control.Monad.Eff.JQuery as J

-- seqname is sometimes header'd as Strain
-- may include more granular location?
-- does disease exist or is it equal to strain?
type RichEntry = { seqname  :: String
                 , acc      :: String
                 , sequence :: String
                 , year     :: Year
                 , month    :: Maybe Month
                 , day      :: Maybe Int
                 , disease  :: Maybe String
                 , country  :: String
                 , host     :: Host
                 , serotype :: Serotype
                 , sequence :: String }
-- serotypes also act as collection names
data Host = Human | Mosquito
data Serotype = DENV1 | DENV2 | DENV3 | DENV4
data Segment = PB1 | PB2 -- ... etc.

-- all these Arrays should go back to being Traversables
-- this function would be safer if it accepted only Bounded instances.
rangeOptions :: forall t57 t61. Array String -> Eff ( dom :: DOM | t57) (Array J.JQuery) 
rangeOptions = traverse makeOption
  where makeOption x = withAttr "<option>" "value" x >>= (J.setText x) >>= return
          
withAttr h f v = J.create h >>= (J.setAttr f v) >>= return 
withId h id = withAttr h "id" id

appendAll  ::    forall t171 t178. (Traversable t171) => J.JQuery -> t171 J.JQuery -> Eff ( dom :: DOM | t178) Unit --(t171 JQuery) 
appendAll x xs = traverse_ (flip J.append x) xs

rangeDropdown id xs = do
   select <- withId "<select>" id
   options <- rangeOptions xs
   appendAll select options
   return select
   
main = J.ready $ do
  body <- J.body
  select <- rangeDropdown "month" (map show $ enumFromTo January December)
  btn  <- J.create "<button>"
  text  <- J.create "<p>" 
  J.setText "unclicked" text
  traverse (flip J.append body) [text, select, btn]
  J.on "click" (handleClick select text) btn
  where
    handleClick input text _ _  = do
      v <- selectId "month"
      J.setText v text

  -- do validation and convert fields to types and put all together into a record(?)
  -- TODO: switch <select> to store an int value, and use with
  -- Data.Enum.toEnum, e.g.
    -- toEnum 0 :: Maybe Month == January
    -- (that validates all the enum fields) 
-- equivalent to handleClick (and will replace it)
handleQuery input text _ _  = do
--year     <- fetch Year "year"
--month    <- fetch Month "month"
--day      <- fetch Int "day"
--host     <- fetch Host "host"
--serotype <- fetch Serotype "serotype"
  acc      <- fetch id "acc"
  disease  <- fetch id "disease"
  country  <- fetch id "country"
  return country
  where 
    fetch f id = f <$> toMaybe <$> selectId id
    --note: strings are not character arrays in purescript
    toMaybe s = if ((trim s) == "") then Nothing else Just s

selectId ::  forall t5. String -> Eff ( dom :: DOM | t5) String 
selectId s = do
   res <- J.select ("#" ++ s)
   --TODO: I don't understand this bit with read.
   Right v <- read <$> J.getValue res
   return v

--   No type class instance was found for . .. can often be cured by providing type signature 
