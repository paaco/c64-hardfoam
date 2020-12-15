; Hard Foam - a 2K card game
; Developed for the https://itch.io/jam/the-c64-cassette-50-charity-competition

; Only WRITES memory < $1000 and uses Dxxx IO, KERNAL and BASIC calls

; Note that it is only required to load below $1000, not specifically $0801,
; so we could even load at $0400 (not lower to keep Tape loading compatibility)
; AND: we may only WRITE <$1000, so reading BASIC/KERNAL would be OK

!source "constants.inc"

*=$0801
!byte $0b,$08,$b6,$07,$9e,$32,$30,$36,$31,$00,$00,$00   ; 1974 SYS2061

;2061
start:
            lda #BLACK
            sta $0286 ; Current Character Color code
            jsr $E544 ; CLS
            lda #BLACK
            sta $D020
            lda #BROWN
            sta $D021

-           jsr $FFE4 ; Get From Keyboard
            beq -
            rts



;----------------------------------------------------------------------------
; DATA
;----------------------------------------------------------------------------

; card layout:
; 5 chars wide, 6 high P---] |/\/| |/\/| |/\/| |   | AA-DD
; the glyph is 3x3 in the suit color
; each card has a name and some text associated with it
; if there's room we can even add flavor text

; 3x3 bytes per glyph (maybe A000- looks nice?)
Glyphs:


; Max 2K
!if * >= $1000 {
    !error "Out of memory"
}
