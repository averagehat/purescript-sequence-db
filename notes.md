```haskell
-- > (+) <$> (Just 1) <*> (Just 2)
-- lift doesn'w work because stuff gets double wrapped. 
-- i.e. head <$> tail xs :: Maybe Maybe a   (so use >>=) 
pair xs = case xs of
  [] -> Just([])
  xs -> lift2 (:) ((tail xs >>= head) >>= (\b -> (head xs) >>= \a -> Just(Tuple a b)))  (tail xs >>= tail >>= pair)

enpair xs = case xs of
  []  -> Just []
  xs -> do
    ys <- tail xs
    zs <- tail ys
    x  <- head xs
    y  <- head ys
    p  <- Tuple x y 
    (:) p (enpair zs)
    
pair xs = case xs of
  [] -> Just([])
  xs -> (:) <$> ((tail xs >>= head) >>= (\a -> (Tuple a) <$> (head xs) )) <*> (tail xs >>= tail >>= pair)

    
--   No type class instance was found for . .. can often be cured by providing type signature
import qualified Halogen.HTML.Indexed as H
rangeOptionsor :: forall a. (Show a) => String -> [a] -> Html
rangeOptionsor s xs = [H.select [P.id_ s] (map (option . show) xs)]
  where option x = H.option [P.value x] 
 [H.ul, [P.id_ "queryForm"]
  [H.input [P.id_ "query"]]
  [H.input [P.id_ "name"]]
  (rangeOptionsor "month" months),
  (rangeOptionsor "year"   (range 1900 2016))
  (rangeOptionsor "day"   (range 1 31))
  [H.input [P.id_ "query"]]
  [H.input [P.id_ "query"]]
  
type AppState = { collection :: String
                 , results   :: Array RichEntry
                 , format    :: Format }
```

at some point add convenience functions to purescript-jquery
&& add a simple Vector-based DSL


```haskell 
_p opts xs = do
  p <- J.create "<p>"
  traverse (\(f,v) -> J.setAttr f v p) $ pair opts 
```
