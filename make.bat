acme -f cbm -DDEBUG=0 -o .cache/hardfoam.prg hardfoam.asm
exomizer sfx $0400 -M256 .cache/hardfoam.prg -o hardfoam.prg
dir hardfoam.prg