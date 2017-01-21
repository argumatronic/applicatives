Slides and examples from Applicative workshop
LambdaConf Winter Retreat, January 2017


# Talk Outline:

## Monad!  
- context dependency: earlier failures can short-circuit later code  
- maybe.hs  
 
## Applicative  
- another type of functor, different from Functor or Monad  
- anagramMaybe.hs  
- compare monadic and applicative versions  

## AccValidation  
- datatype from `validation` library 
- anagramEither.hs 
- like `Either` but can accumulate error messages on the left  
- `acc` project

## ApplicativeDo  
- new language extension lets you use `do` syntax with applicatives   
- see applicDo.hs within `acc` project  

## We didn't talk much about parsing but probably should have 

## `optparse-applicative`  
- library for command line argument parsing  
- most of it is implemented applicatively (not monadically, though monadic parsing is also fun)  
- we didn't talk about how most of the functions and types this library provides works  
- walked through example `optex` project  

## using `optparse` in a project  
- command line address book along the lines of ppl addressbook  
- this is just the argument parsing portion  
- `address` project

