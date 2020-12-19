acme -f cbm -DDEBUG=0 -o .cache/hardfoamp.prg hardfoam.asm
exomizer sfx $0400 -M256 .cache/hardfoamp.prg -o hardfoam.prg
dir hardfoam.prg