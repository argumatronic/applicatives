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
  case noEmpty name' of
   Nothing -> Nothing
   Just namae ->
     case noNegative age' of
      Nothing -> Nothing
      Just aged ->
        case noEmpty pl' of
          Nothing -> Nothing
          Just langy ->
              plCheck (Person namae aged langy)

-- `do` syntax isn't just for IO.

mkPerson' :: String -> Int -> String -> Maybe Person
mkPerson' name' age' pl' = do
  namae <- noEmpty name'
  aged <- noNegative age'
  langy <- noEmpty pl'
  plCheck (Person namae aged langy)

-- monad context even though there's no IO here, 
-- because later computations depend on the result of earlier ones

-- mkPerson "Simon" 45 "Scala"
-- mkPerson "Chris" (-30) "Scala"

-- another version with applicative and bind
mkPer :: String -> Int -> String -> Maybe Person
mkPer name' age' pl' = 
  Person <$> (nonEmpty name') 
         <*> (nonNegative age') 
         <*> (nonEmpty pl') 
  >>= plCheck
