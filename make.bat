acme -f cbm -DDEBUG=0 -o .cache/hardfoam-release.prg hardfoam.asm
exomizer sfx sys .cache/hardfoam-release.prg -o hardfoam.exo.prg
alz64 -s -1 $37 .cache\hardfoam-release.prg hardfoam.alz.prg
dir hardfoam*.prg
