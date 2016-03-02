module Main where

import Prelude
import Control.Monad.Eff
import Control.Monad.Eff.Console
import Data.Date -- https://github.com/purescript/purescript-datetime
import Data.Maybe
data Serotype = DENV1 | DENV2 | DENV3 | DENV4
data Host = Human | Mosquito
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

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  log "Hello sai!"
