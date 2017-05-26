mkPer' :: String -> Int -> String -> Maybe Person
mkPer' name' age' pl' = 
  Person <$> (nonEmpty name')   
  -- <$> is infix operator for `fmap`
  -- nonEmpty name -> Maybe (Person String)
         <*> (nonNegative age') 
         -- <*> is the applicative operator :: f (a -> b) -> f a -> f b
         -- Maybe (Person String Int)
         <*> (nonEmpty pl') 
         -- Maybe (Person String Int String)
  >>= plCheck
  -- plCheck :: Person -> Maybe Person

  >>= :: m a -> (a -> m b) -> m b
  m a ~ Maybe Person value resulting from applicative block
  a -> m b ~ plCheck ~ Person -> Maybe Person
  m b ~ Maybe Person

  Maybe (Maybe Person) ~ join :: m (m a) -> m a
  >>= = fmap . join