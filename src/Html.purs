data Attr = Attr String String

appendAll  ::    forall t171 t178. (Traversable t171) => J.JQuery -> t171 J.JQuery -> Eff ( dom :: DOM | t178) Unit --(t171 JQuery) 
appendAll x xs = traverse_ (flip J.append x) xs

withAttr h (Attr f v) = J.create h >>= (J.setAttr f v) >>= return
data Node = P []

ul :: Attr -> [HTML] -> HTML
ul attr elems = do
  e <- withAttr "<ul>" attr
  appendAll e elems

ul :: [HTML] -> HTML
ul_ elems = do
  e <- J.create "<ul>"
  appendAll e elems

makeElem :: String -> Attr -> [HTML] -> HTML
makElem tag attr elems = do
  e <- withAttr tag attr
  appendAll e elems


makeElem_ :: String ->  [HTML] -> HTML
makElem_ tag attr elems = do
  e <- J.create tag
  appendAll e elems



tags = ["ul", "li", "p", "button", "input"]

attrs = ["id", "class"]

t ++ " = makeElem " ++ "\"" t ++
