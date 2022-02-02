/*
    Print glyphs from cards.c as acme source code

    Compile with: tcc getglyphs.c
*/

#include <stdio.h>
#include "assets/cards.c"

int MAXGLYPHS=32;

void main()
{
    printf("GlyphData:\n");
    for (int i=0; i<MAXGLYPHS; i++) {
        printf("    G_%d=*=GlyphData\n", i+1);
        int xo = (i % 8) * 5;
        int yo = (i / 8) * 6 * 40;
        int offset = 2 + 41 + xo + yo;
        for (int y=0; y<3; y++) {
            printf("    !byte ");
            for (int x=0; x<3; x++) {
                if (x != 0) printf(",");
                printf("%d", frame0000[offset + x + y * 40]);
            }
            printf("\n");
        }
    }
}
