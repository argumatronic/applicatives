-- this is the same idea as maybe.hs but
-- uses Either instead of Maybe

data Person = Person {
      name   :: String
    , age    :: Int
    , pl     :: String
   } deriving (Eq, Show)

-- these can remain Maybe and we'll match on their results later
nonEmpty :: String -> Maybe String
nonEmpty ""  = Nothing
nonEmpty str = Just str

nonNegative :: Int -> Maybe Int
nonNegative n = if n >= 0 then Just n else Nothing


plCheck :: Person -> Either String Person
plCheck c =
  if (name c == "Simon") && ((pl c) /= "Haskell")
     then Left "All Simons write Haskell."
     else Right c

-- using Either instead of Maybe gives a better demonstration
-- of where it's failing and how it doesn't "see" any later error 
-- once it's already hit a Left case.
mkPerson1 :: String
            -> Int
            -> String
            -> Either String Person
mkPerson1 name' age' pl' =
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
              plCheck (Person named aged lang)

-- and, again, we can rewrite this:
mkPerson2 :: String -> Int -> String -> Either String Person
mkPerson2 name' age' pl' = do
  named <- noEmpty name'
  aged <- noNegative age'
  lang <- noEmpty pl'
  plCheck (Person named aged lang)



-- again, this is overkill but what the heck! 
-- we'll do it another way, with Either everywhere.

noEmpty :: String -> Either String String
noEmpty ""  = Left "Empty string."
noEmpty str = Right str

noNegative :: Int -> Either String Int
noNegative n = if n >= 0 then Right n else Left "Negative age."

-- another version with applicative and bind


mkPerson3 :: String -> Int -> String -> Either String Person
mkPerson3 name' age' pl' = 
  Person <$> (noEmpty name') 
         <*> (noNegative age') 
         <*> (noEmpty pl') 
  >>= plCheck
-- but try changing this to use to the Maybe versions (nonEmpty, nonNegative)