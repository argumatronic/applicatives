data Person = Person {
      name   :: String
    , age    :: Int
    , pl     :: String
   } deriving (Eq, Show)

nonEmpty :: String -> Maybe String
nonEmpty ""  = Nothing
nonEmpty str = Just str

nonNegative :: Int -> Maybe Int
nonNegative n | n >= 0 = Just n
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

mkPerson' :: String -> Int -> String -> Maybe Person
mkPerson' name' age' pl' = do
  namae <- nonEmpty name'
  aged <- nonNegative age'
  langy <- nonEmpty pl'
  plCheck (Person namae aged langy)

plChk :: Person -> Either String Person
plChk c =
  let p = pl c
      n = name c
  in if n == "Simon" && (p /= "Haskell")
     then Left "All Simons write Haskell."
     else Right c

-- using Either instead of Maybe gives a better demonstration
-- of where it's failing and how it doesn't "see" any later error 
-- once it's already hit a Left case.
mkPer :: String
            -> Int
            -> String
            -> Either String Person
mkPer name' age' pl' =
  case nonEmpty name' of
   Nothing -> Left "Empty name value."
   -- if name is empty, it will tell us that
   -- if it's not empty, it returns Just name and goes on to
   -- the next check
   Just named ->
     case nonNegative age' of
      Nothing -> Left "Negative age."
      -- if a negative number, we get Nothing; otherwise, go on
      Just aged ->
        case nonEmpty pl' of
          Nothing -> Left "Empty prog lang."
          -- and again check for empty strings
          Just lang ->
              plChk (Person named aged lang)

-- monad context even though there's no IO here, 
-- because later computations depend on the result of earlier ones

-- mkPer "Simon" (-3) ""
-- Left "Negative age."

per :: String -> Int -> String -> Maybe Person
per name' age' pl' = Person <$> (nonEmpty name') 
                            <*> (nonNegative age') 
                            <*> (nonEmpty pl')

-- desugaring `do` with applicative and bind
mkPer' :: String -> Int -> String -> Maybe Person
mkPer' name' age' pl' = 
  -- Person <$> (nonEmpty name') 
  --        <*> (nonNegative age') 
  --        <*> (nonEmpty pl') 
  per name' age' pl' >>= plCheck


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

-- another version with applicative and bind

-- mkPer :: String -> Int -> String -> Maybe Person
-- mkPer name' age' pl' = 
--   Person <$> (noEmpty name') 
--          <*> (noNegative age') 
--          <*> (noEmpty pl') 
--   >>= plCheck

mkP :: String -> Int -> String -> Either String Person
mkP name' age' pl' = 
  Person <$> (noEmpty name') 
         <*> (noNegative age') 
         <*> (noEmpty pl') 
  >>= plChk

