module TextGeneral (idleOptions,pressEnter,exitGame,enterName,moveOptions) where
-- General text

pressEnter = "Welcome! press enter to start!"
enterName = "Please enter a name for our Hero!:"
idleOptions = "Do you want to:\n\tMove (m)\n\tRest (r)\n\tQuit (q)"
exitGame = "Are you sure you want to quit?\nAll progress will be lost (y/n): "
moveOptions = "Moved path & Walls: | -\nStarting position: S\nPlayer: P"