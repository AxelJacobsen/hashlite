# HashLite

## How to run
Running "stack run" in root will build and launch the game. However if you want to use flags, then it has to be ran via the exe file. <br />This can be located at: <br />
".stack-work\install\c0c17729\bin\hashlite-exe.exe" or <br />
".stack-work\dist\d53b6a14\build\hashlite-exe\hashlite-exe.exe"<br />
Implemented flags: ["--help","-help","help","--h","-h","h"]

# Project Description
The game is a simple Rougelike where you fight monsters, level up and advance the dungeon. <br />
Your goal is to fight your way to the bottom, and discover who awaits at the depths (layer 100). <br />
All the controls are presented ingame and should be pretty self explanatory, even for inexpirienced players.<br /> 
You gain EXP from killing monsters and advancing layers. <br />
When you level up you can upgrade your weapon or increase your armour. <br />
On the map you can encounter monsters, merchants and loot rooms.

## Initial idea
The plan is to make a very basic "roguelite" esq game. <br />
It will be mostly text based with a simple "visual" representation of the available choices. <br />
The goal would be to have a fully randomized map with; combat, character encounters (crude), loot rooms and a boss. <br />

## Missing features
Encounters: <br />
Shop encounter <br />
People encounter <br />

## Testing
To run tests do "stack test" in root. The tests are all located in the "test" folder.

## Balancing
In the game things are designed to scale according to how far you have gotten, though i havent had the time to properly balance this. <br />
In other words, the start is usually gruelingly difficult, and at some point enemies die to one hit. <br />
This is simply the state of the game, since the intent isn't for players to actually play till floot 100.

# Spoiler
<br /><br />
The bossfight is a refrence to the famous Norwegian gold-medalist Oddvar Brå.<br />
I couldnt get myself to translate his frases as they simply arent funny in english.<br />
It was never intended for anyone to actually reach the bottom, and he exists more as an easteregg than and actual boss.<br />