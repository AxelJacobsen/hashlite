module TextGeneral (wantRest, restDanger, restNormal, restSafe, levelUp1, levelUp2, continue, idleOptionsOne,idleOptionsTwo,pressEnter,exitGame,enterName,moveSymbols,moveOptions,moveIllegal,healMessage,outOfHeal,playerDeath1,playerDeath2,playerDeath3,playerDeath4) where
-- General text

pressEnter = "Welcome! press enter to start!"
enterName = "Please enter a name for our Hero!:"
idleOptionsOne = "What do you want to do "
idleOptionsTwo = "\n\tMove (m)\n\tRest (r)\n\tQuit (q)"
exitGame = "Are you sure you want to quit?\nAll progress will be lost (y/n): "
playerDeath1 = " has died...\n"
playerDeath2 = " gathered: "
playerDeath3 = " coins,\nand got to layer: "
playerDeath4 = ".\nBetter luck next time!"
moveSymbols = "Moved path & Walls: | - +\nStarting position: S\nPlayer: P"
moveOptions = "Where do you want to walk? (w = up, a = left, s = down, d = right, q = stop moving):"
moveIllegal = "Illegal direction."
healMessage = "You have healed for "
outOfHeal = "You are out of healing potions!"
continue = "Press enter to continue..."
levelUp1 = "You have leveled up, "
levelUp2 = "! MAXHP increased.\nDo you want to;\n(s) Sharpen your sword,\n(a) Strengthen your armour?"
restSafe = "You feel safe here..."
restNormal = "There isn't anyone around..."
restDanger = "You feel as if you are being watched..."
wantRest = "\nDo you want to rest? (y/n):"