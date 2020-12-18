# c64-hardfoam
Cassette 50 charity competition entry

https://itch.io/jam/the-c64-cassette-50-charity-competition

Technical limitations:

 Final code must be in .PRG format that loads into memory below $1000 hex, giving 4,096 bytes of address space.
 At no point can the code write to any location at or above $1000, however using IO normally at $D000-$DFFF is allowed, as is calling BASIC or KERNAL routines. BASIC programs are welcome too, just be sure to start your program with POKE 56,16 so it conforms to the memory limits.

# HARD FOAM

The game "Hard Foam" will be a card game like Hearthstone in PETSCII.

Cards will be 5x6 characters with a 3x3 glyph, a number of energy to use and attack and values.

Background is dark grey.

There will be 4 "suits":
1. Goblin (black) - who doesn't like goblins?
1. Polystyrene (white) - the most widely used hard foam plastic in the world
1. Candy (red) - from "hard candy", the Madonna album
1. Soap (cyan) - for the "foam", soapSTONE is a soft kind of stone used for sculpting

There will be 2 types of cards: monsters and spells.

There will be 7 cards of each suit, with one of them legendary (yellow border).

To create a deck:
1. select a legendary
1. pick 4 of that color
1. pick 3 of the color black

If you choose the black legendary, your deck will automatically consist of all blacks.

The deck will consist of the 7 x 4 selected cards, with 1 randomly replaced by the legendary.
