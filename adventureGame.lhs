> -- main game loop
> -- when user inputs a character corresponding to a direction or an action
> -- (in the proper location), the corresponding game dialogue runs

> gameLoop :: Game -> IO Game
> gameLoop (location, character, s) = do -- you can change these


>  -- game output
>  -- some ASCII ART for fun
>  -- but be carefull, some characters need escaping in string literals '\' (for example '\')
>     putStrLn ("                               __    _  _ _  _  _                 ")
>     putStrLn ("                              /  | ||_)|_)\\\\//   \\                      ")
>     putStrLn ("               /              \\__|_|| \\| \\ ||    /\\_                   /   ")
>     putStrLn ("              /             |  GAMES & Coffee store  |                /    ")
>     putStrLn ("             /______________||______________________||_______________/     ")

>     putStrLn ("            |_|_|_|_|_|_|_|_|_|_|_|\\/||\\/||\\/|_|_|_|_|_|_|_|_|_|_|_|||     ")

>     putStrLn ("            |_|_;;;;;;;;;_|_|_|_|_|/\\||/\\||/\\|_|_|_|_|_|_;;;;;;;;;_||| /|  ")

>     putStrLn ("            |_|_;;;;;;;;;_|_|_|_|_|\\/||\\/||\\/|_|_|_|_|_|_;;;;;;;;;_|||/||  ")

>     putStrLn ("            |_|_;;;;;;;;;_|_|_|_|_|/\\||/\\||/\\|_|_|_|_|_|_;;;;;;;;;_||||/|  ")

>     putStrLn ("            |_|_|_|_|_|_|_|_|_|     _      _     |_|_|_|_|_|_|_|_|_|_|/||  ")

>     putStrLn ("            |_| GAMES         |    (_)    (_)    |                 |_|/||  ")

>     putStrLn ("            |_|.         *    |__________________|                 |_||/|  ")

>     putStrLn ("            |_|*`.      ***   |_|      ||      |_|     COFFEE      |_|/||  ")

>     putStrLn ("            |_| S `.   *****  |_| lluq || push |_|                 |_||/|  ")

>     putStrLn ("            |_|`. A `.******* |_| tuo  ||  in  |_|                 |_|/||")

>     putStrLn ("            |_|  `. L*`.******|_|     [||]     |_|  _____   _   ___|_||/| ")

>     putStrLn ("            |_|    `.*E*`.****|_|      ||      |_| /    /  //  /   |_|/||   /\\/\\")

>     putStrLn ("            |_|______`__*||___|_|      ||      |_|/_OO_/__//__/_OO_|_||/|   >^^< ")

>     putStrLn ("            |_|_|_|_|_|_|_|_|_|_|______||______|_|_|_|_|_|_|_|_|_|_|_|/||    /\\ ")

>     putStrLn ("            |_|_|_|_|_|_|_|_|_|_|______||______|_|_|_|_|_|_|_|_|_|_|_||/_   (__)__")

>     putStrLn ("           /     /     /     /     /     /     /     /     /     /     /     / ")

>     putStrLn ("          /_____/_____/_____/_____/_____/_____/_____/_____/_____/_____/_____/ ")

>     putStrLn ("          __________________________________________________________________ ")

>     putStrLn ("                                                               /_/_/_/ ")



>     putStrLn ("You are in " ++ location) -- ++ location)
>     putStrLn ("You are " ++ character) -- charater
>     putStrLn ("You can travel to Haskell-Sea, but watch out, Lambda-Monsters are said to lurk around!") -- ++ new_location 
>     putStrLn ("Anyway, we hope that maybe you can see some magic - functionally - happening.") -- an object maybe, a location ahead

>     putStrLn (" ")

>     -- user input
>     input <- getLine

>     putStrLn (input) -- return what was the input

>   -- input analysis
>     if input `elem` quitCharacter -- user wants to quit the game
>     then do
>        let  g = (location, character ,s)
>        return (g)
>     else do
>    --     if  -- input = choice of travel,
>    --        then do
>     --            putStrLn (" ")
>    --             gameLoop (new_loc,o,c) -- start game loop with new location
>    --        else do -- input == choice of action
>    --             updated_game
>   --              putStrLn (" ")
>    --        gameLoop (updated_game)
>         gameLoop(location, character, s)


> -- starts the game loop with the initial Game
> game :: IO ()
> game = do
>     gameLoop start
>     return ()

> -- input that exits the game
> quitCharacter = [":q", ":Q", ":e", ":E"] 

> -- ------------------- GAME ---------------------

> type Game  = ([Char],[Char], Char) -- you can modify the type

> start :: Game
> start =  ("Haskell-Ville" , "Harry Curry", 'C') -- to be changed

> ------------- data types ----------------

> -- data Direction = ..

> -- data Movement = TurnLeft 

> type Location = String

