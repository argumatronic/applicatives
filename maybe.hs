data Person = Person {
      name   :: String
    , age    :: Int
    , pl     :: String
   } deriving (Eq, Show)

noEmpty :: String -> Maybe String
noEmpty ""  = Nothing
noEmpty str = Just str

noNegative :: Int -> Maybe Int
noNegative n | n >= 0 = Just n
             | otherwise = Nothing

plCheck :: Person -> Maybe Person
plCheck c =
  let p = pl c
      n = name c
  in if n == "Simon" && (p /= "Haskell")
     then Nothing
     else Just c

mkPerson :: String
            -> Int
            -> String
            -> Maybe Person
mkPerson name' age' pl' =
  -- have the ' to distinguish from the record-accessor
  -- functions in the record datatype
  case noEmpty name' of
   Nothing -> Nothing
   Just name' ->
     case noNegative age' of
      Nothing -> Nothing
      Just age' ->
        case noEmpty pl' of
          Nothing -> Nothing
          Just pl' ->
              plCheck (Person name' age' pl')

-- `do` syntax isn't just for IO; 
-- Maybe is also a Monad
-- and later computations (specifically plCheck)
-- do depend on results of earlier ones

-- mkPerson' :: String -> Int -> String -> Either String Person
-- mkPerson' name' age' pl' = do
--   namae <- noEmpty name'
--   aged <- noNegative age'
--   langy <- noEmpty pl'
--   plCheck (Person name' age' pl')

-- mkPerson "Simon" 45 "Scala"
-- mkPerson "Chris" (-30) "Scala"

-- another version with applicative and bind

mkPer :: String -> Int -> String -> Maybe Person
mkPer name' age' pl' = 
  Person <$> (noEmpty name') 
         <*> (noNegative age') 
         <*> (noEmpty pl') 
  >>= plCheck
