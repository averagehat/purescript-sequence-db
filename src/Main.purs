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
import Control.Apply
import Data.Foldable
import Data.Array 
import Data.Foreign.Class (read)
import Data.Enum (enumFromTo)
import Data.Tuple
import qualified Halogen.HTML.Properties.Indexed as P
import Control.Monad.Eff.JQuery as J
data Serotype = DENV1 | DENV2 | DENV3 | DENV4
data Host = Human | Mosquito
foo :: Maybe Int
foo = Just 1

--rangeSelector s xs = do
s = "foo"
month = January

rangeSelect :: forall t2. J.JQuery -> Array String -> Eff (dom :: DOM
                                                   | t2
                                                   )
                                               J.JQuery 
rangeSelect select xs = J.ready $ do
  traverse makeOption xs
  where
    makeOption s = J.ready $ do
          option <- J.create "<option>"
          J.setText  s option
          J.setAttr "value" s option
          J.append option select
          return option
-- > (+) <$> (Just 1) <*> (Just 2)
-- lift doesn'w work because stuff gets double wrapped. 
-- i.e. head <$> tail xs :: Maybe Maybe a   (so use >>=)
pair xs = case xs of
  [] -> Just([])
  xs -> lift2 (:) ((tail xs >>= head) >>= (\b -> (head xs) >>= \a -> Just(Tuple a b)))  (tail xs >>= tail >>= pair)

pair' xs = case xs of
  [] -> Just([])
  xs -> (:) <$> ((tail xs >>= head) >>= (\a -> (Tuple a) <$> (head xs) )) <*> (tail xs >>= tail >>= pair')
-- pair xs = (lift2 Tuple (head xs) (head <$> tail xs)) (pair $ tail <$> tail xs)
--_p opts xs = do
--  p <- J.create "<p>"
--  traverse (\(f,v) -> J.setAttr f v p) $ pair opts
withId h id = J.create h >>= (J.setAttr "id" s) >>= return
main = J.ready $ do
  body <- J.body
  select <- withId "<select>" "month"
  -- rangeSelect also adds all options as select's children
  options <- rangeSelect select $ map show $ enumFromTo January December
  text  <- J.create "<p>" 
  btn  <- J.create "<button>"
  -- these haven't been created yet
  J.setText "unclicked" text
  traverse (flip J.append body) [text, select, btn]
--  J.append text body
--  J.append select body
--  J.append btn body
  J.on "click" (handleClick select text) btn
  where
    handleClick input text _ _  = do
      v <- selectValue s
      J.setText v text

--   No type class instance was found for . .. can often be cured by providing type signature
selectValue ::  forall t5. String -> Eff ( dom :: DOM | t5) String 
selectValue s = do
   res <- J.select ("#" ++ s)
   Right v <- read <$> J.getValue res
   return v

     --selected <- J.select ("#" ++ s)
--  J.on "change" (handleChange select text) select
--  where
--  handleChange input text _ _ = do
--    Right name <- read <$> J.getValue input
--    log $ "Name changed to " ++ name
--    J.setText ("Hello, " ++ name) text
  
  
  
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
