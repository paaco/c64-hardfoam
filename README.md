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
1. polystyrene (white) - the most widely used plastic in the world
1. cobalt (black) - used in batteries, mined by children, also exists in Vitamin B12
1. candy (red) - like "hard candy" from madonna
1. soapstone (cyan) - for the "foam", this is a softer kind of stone used to make counters and kitchen desks and malls

There will be 2 types of cards: monsters and spells.

There will be 8 cards of each suit, with 1 legendary (yellow border).
Max 2 of each. A legendary can be used only once.
