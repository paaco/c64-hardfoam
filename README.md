# c64-hardfoam
Cassette 50 charity competition entry

https://itch.io/jam/the-c64-cassette-50-charity-competition

Technical limitations:

 Final code must be in .PRG format that loads into memory below $1000 hex, giving 4,096 bytes of address space.
 At no point can the code write to any location at or above $1000, however using IO normally at $D000-$DFFF is allowed, as is calling BASIC or KERNAL routines. BASIC programs are welcome too, just be sure to start your program with POKE 56,16 so it conforms to the memory limits.

# HARD FOAM

The game "HARD FOAM" will be a card game like Hearthstone in PETSCII.

Cards will be 5x6 characters with a 3x3 glyph, a number of energy to use and have attack and defense values.

Background is blue to accomodate the suit colors.

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


# Memory Map

*=$0120     ; DATA (0120-01ED = 282 bytes)

*=$01ED     ; DATA 13 bytes including return address (TRASHED WHILE LOADING)

*=$01FA     ; DATA (01FA-0314 = 282 bytes)

*=$028D     ; 028D-028E (2 bytes) TRASHED DURING LOADING

*=$02A1     ; RS232 Enables SHOULD STAY 0 DURING LOADING!

*=$0314     ; IRQ, BRK and NMI Vectors to keep

*=$032A     ; DATA (032A-0400 = 214 bytes)

*=$0590     ; DATA (200 bytes, MIDDLE 5 SCREEN LINES ARE HIDDEN)

*=$07E8     ; CODE

*=$1000     ; EOF