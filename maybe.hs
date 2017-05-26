data Person = Person {
      name   :: String
    , age    :: Int
    , pl     :: String
   } deriving (Eq, Show)

nonEmpty :: String -> Maybe String
-- pattern match; yes, there are other ways to do it
nonEmpty ""  = Nothing
nonEmpty str = Just str

nonNegative :: Int -> Maybe Int
nonNegative n = if n >= 0 then Just n else Nothing

plCheck :: Person -> Maybe Person
plCheck c =
  if (name c) == "Simon" && ((pl c) /= "Haskell")
     then Nothing
     else Just c
  -- `name` and `pl` here are those record accessor functions from our record type

mkPerson :: String
            -> Int
            -> String
            -> Maybe Person
mkPerson name' age' pl' =
  -- have the ' to distinguish from the record-accessor
  -- functions in the record datatype
  case nonEmpty name' of
   Nothing -> Nothing
   Just name'' ->
     case nonNegative age' of
      Nothing -> Nothing
      Just age'' ->
        case nonEmpty pl' of
          Nothing -> Nothing
          Just pl'' ->
              plCheck (Person name'' age'' pl'')
          -- note that if any of the above returns a `Nothing`, we will never make it to the `plCheck`

-- `do` syntax isn't just for IO; 
-- Maybe is also a Monad
-- and later computations (specifically plCheck)
-- do depend on results of earlier ones
-- following is same as above, but ~monadic~
mkPerson' :: String -> Int -> String -> Maybe Person
mkPerson' name' age' pl' = do
  name'' <- nonEmpty name'
  age'' <- nonNegative age'
  lang'' <- nonEmpty pl'
  plCheck (Person name'' age'' lang'')


-- desugaring `do` with applicative and bind
-- first we construct a Person value by applying the Person type constructor
-- to the results of the three function/values and then pass that as the (m a) to (>>=)
-- where the `m` is Maybe, and the (a -> m b) is plCheck (:: Person -> Maybe Person)
mkPer' :: String -> Int -> String -> Maybe Person
mkPer' name' age' pl' = 
  -- Person <$> (nonEmpty name') 
  --        <*> (nonNegative age') 
  --        <*> (nonEmpty pl') 
  per name' age' pl' >>= plCheck
  -- Maybe Person >>= (Person -> Maybe Person)


noEmpty :: String -> Either String String
noEmpty ""  = Left "Empty string."
noEmpty str = Right str

noNegative :: Int -> Either String Int
noNegative n | n >= 0 = Right n
              | otherwise = Left "Negative age."

-- plChk :: Person -> Either String Person
-- plChk c =
--   let p = pl c
--       n = name c
--   in if n == "Simon" && (p /= "Haskell")
--      then Left "All Simons write Haskell."
--      else Right c

  Person <$> (nonEmpty name')   -- <$> is infix operator for `fmap`
         <*> (nonNegative age') -- <*> is the applicative operator :: f (a -> b) -> f a -> f b
         <*> (nonEmpty pl') 
  >>= plCheck
-- >>= is called "bind" and is the main operator in Monad :: m a -> (a -> m b) -> m b

-- this is overkill but if we factor out the applicative
-- part, it's easier to see the intermediate type signature
per :: String -> Int -> String -> Maybe Person
per name' age' pl' = Person <$> (nonEmpty name')   
                            <*> (nonNegative age') 
                            <*> (nonEmpty pl')

mkP :: String -> Int -> String -> Maybe Person
mkP name' age' pl' = 
  per name' age' pl' >>= plCheck  



