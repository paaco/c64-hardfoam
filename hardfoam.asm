; Hard Foam - a 2K card game
; Developed for the https://itch.io/jam/the-c64-cassette-50-charity-competition

; Only WRITES memory < $1000 and uses Dxxx IO, KERNAL and BASIC calls

; Note that it is only required to load below $1000, not specifically $0801,
; so we could even load at $0400 (not lower to keep Tape loading compatibility)
; However, loading it there (anything below $0801) will kill RUN, only allow direct SYS
; Exomizer also uses $0334-$03D0 as decrunching buffer; decrunching there will hang
; AND: we may only WRITE <$1000, so reading BASIC/KERNAL would be OK

DEBUG=1

!source "constants.inc"

*=$0801
!if DEBUG=0 {
    !byte $0b,$08,$b6,$07,$9e,$32,$30,$36,$31,$00,$00,$00   ; 1974 SYS2061
} else {
    !initmem $74
    ; jump to developer init
    !byte $0b,$08,$00,$13,$9e,$34,$38,$36,$34,$00,$00,$00   ; 4864 SYS4864
}

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

; TODO draw a card
Draw_card:
            ; manually requires 5 bytes per 'put' or minimum 4 if you use ZP pointers
            lda #79
            sta $0400
            ; looping requires 2+3+3+1+2=11 or minimum 9 if you use ZP pointers
            ; so that's gonna be a win for more than 3 puts
            ldx #5
-           lda Glyphs,x
            sta $0400,x
            dex
            bne -


;----------------------------------------------------------------------------
; DATA
;----------------------------------------------------------------------------

; card layout:
; 5 chars wide, 6 high P---] |/\/| |/\/| |/\/| |   | AA-DD
; 79,119,119,119,80
; 116,0,0,0,106 x4 ;; ;HE? 101==116 en 106==103 WTF? zelfs in LOWER CASE?
; 76,111,111,111,122
; the glyph is 3x3 in the suit color
; each card has a name and some text associated with it
; if there's room we can even add flavor text

; 3x3 bytes per glyph (maybe A000- looks nice?)
Glyphs:


; Max 2K
!if * >= $1000 {
    !error "Out of memory"
}


; init code copy loop
!if DEBUG=1 {
*=$1300
        ldx #$100-$D0-1
-       lda $13D0,x
        sta $03D0,x
        dex
        bpl -
        ldx #0
-       lda $1400,x
        sta $0400,x
        lda $1500,x
        sta $0500,x
        lda $1600,x
        sta $0600,x
        lda $1700,x
        sta $0700,x
        inx
        bne -
        jmp INIT
}

;----------------------------------------------------------------------------
; INIT CODE (03D0-07FF)
;----------------------------------------------------------------------------
!if DEBUG=0 {
    *=$03D0
} else {
    *=$13D0 ; 5072
}
!pseudopc $03D0 {
INIT:
        ; set up ZP pointers
        rts
}
!fill $0800-(*&$0FFF)-1,$74
!byte $FF ; should be at $07FF
