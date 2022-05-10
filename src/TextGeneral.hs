module TextGeneral (idleOptionsOne,idleOptionsTwo,pressEnter,exitGame,enterName,moveSymbols,moveOptions,moveIllegal) where
-- General text

pressEnter = "Welcome! press enter to start!"
enterName = "Please enter a name for our Hero!:"
idleOptionsOne = "What do you want to do "
idleOptionsTwo = "\n\tMove (m)\n\tRest (r)\n\tQuit (q)"
exitGame = "Are you sure you want to quit?\nAll progress will be lost (y/n): "
moveSymbols = "Moved path & Walls: | -\nStarting position: S\nPlayer: P"
moveOptions = "Where do you want to walk? (w = up, a = left, s = down, d = right):"
moveIllegal = "Illegal direction."