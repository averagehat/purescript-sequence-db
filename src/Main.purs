module Main where

import Prelude
import qualified Halogen.HTML.Indexed as H
import Data.Either 
import Control.Monad.Eff
import Control.Monad.Eff.Console
import DOM
import Data.Date -- https://github.com/purescript/purescript-datetime
import Data.Maybe
import Data.Traversable
import Data.Foldable
import Data.Foreign.Class (read)
import Data.Enum (enumFromTo)
import qualified Halogen.HTML.Properties.Indexed as P
import Control.Monad.Eff.JQuery as J
data Serotype = DENV1 | DENV2 | DENV3 | DENV4
data Host = Human | Mosquito
foo :: Maybe Int
foo = Just 1

--rangeSelector s xs = do
s = "foo"
month = January
--  flip (J.on "change") input $ \_ _ -> do
--    Right msg <- read <$> J.getValue input
--    J.setText ("send: " ++ msg)
--rangeSelect :: forall a. (Show a, Traversable t) => String -> [a] -> J
--rangeSelect :: forall t t2. (Traversable t) =>  String -> t -> Eff (dom :: DOM
--                                                   | t2
--                                                   )
--                                               J.JQuery 
rangeSelect :: forall t2. J.JQuery ->  String -> Array String -> Eff (dom :: DOM
                                                   | t2
                                                   )
                                               J.JQuery 
rangeSelect select s xs = J.ready $ do
  traverse makeOption  xs
  --select 
  where
    makeOption s = J.ready $ do
          option <- J.create "<option>"
          J.setText  s option
          J.setAttr "value" s option
          J.append option select
          return option
          
main = J.ready $ do
  body <- J.body
  select <- J.create "<select>"
  J.setAttr "id" s select
  options <- rangeSelect select "month" $ map show $ enumFromTo January December
  --traverse (\o -> J.append o select) options
  text  <- J.create "<p>"
  J.setText "BAR" text
  J.append text body
  J.append select body
  
  
  
--main = J.ready $ do 
--  body <- J.body
--  select <- J.create "<select>"
--  J.setAttr "id" s select
--  option <- J.create "<option>"
--  J.setText (show month) option
--  J.setAttr "value" month option
--  text  <- J.create "<p>"
--  J.setText "BAR" text
--  J.append option select
--  J.append select body
--  J.append text body
  
--  J.on "change" (handleChange select text) select
--  where
--  handleChange input text _ _ = do
--    Right name <- read <$> J.getValue input
--    log $ "Name changed to " ++ name
--    J.setText ("Hello, " ++ name) text
--months = enumFromTo January December
--rangeSelector :: forall a. (Show a) => String -> [a] -> Html
--rangeSelector s xs = [H.select [P.id_ s] (map (option . show) xs)]
--  where option x = H.option [P.value x] 
-- [H.ul, [P.id_ "queryForm"]
--  [H.input [P.id_ "query"]]
--  [H.input [P.id_ "name"]]
--  (rangeSelector "month" months),
--  (rangeSelector "year"   (range 1900 2016))
--  (rangeSelector "day"   (range 1 31))
--  [H.input [P.id_ "query"]]
--  [H.input [P.id_ "query"]]
       
-- seqname is sometimes header'd as Strain
-- may include more granular location? 
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
type AppState = { collection :: String
                 , results   :: Array RichEntry
                 , format    :: Format }

data Format = Fasta | Record

-- foo
--main :: forall e. Eff (console :: CONSOLE | e) Unit
--main = do
--  log "Hello sai!"
