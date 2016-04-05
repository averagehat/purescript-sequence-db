
```haskell
  No type class instance was found for

    Prelude.Ord _57


 less :: forall t1 t2. (Ord t1 t2) => t1 -> t2 -> Boolean
 less x y = x < y
  Could not match kind

    *

  with kind

    u37 -> u39


while checking the kind of (Ord t1 t2) => t1 -> t2 -> Boolean
while checking the kind of forall t1. (Ord t1 t2) => t1 -> t2 -> Boolean
while checking the kind of forall t1 t2. (Ord t1 t2) => t1 -> t2 -> Boolean
while checking the kind of forall t1 t2. (Ord t1 t2) => t1 -> t2 -> Boolean
in value declaration less

 between' :: forall a b c. (Ord a b c) => a -> b -> c -> Boolean
 between' min max x = (min <= x) && (x <= max)
  No type class instance was found for

    Prelude.Ord _97
```
