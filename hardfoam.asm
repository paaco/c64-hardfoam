; Hard Foam - a 2K card game
; Developed for the https://itch.io/jam/the-c64-cassette-50-charity-competition

; Only WRITES memory < $1000 and uses Dxxx IO, KERNAL and BASIC calls

; Note that it is only required to load below $1000, not specifically $0801,
; so we could even load at $0400 (not lower to keep Tape loading compatibility)
; However, loading it there (anything below $0801) will kill RUN, only allow direct SYS
; Exomizer also uses $0334-$03D0 as decrunching buffer; decrunching there will hang
; AND: we may only WRITE <$1000, so reading BASIC/KERNAL would be OK

; Usable RAM:  $0200-$0258 / $0293-$02FF = $58+$6C = 196 bytes
; Usable INIT: $03D0-$03FF / $0400-$07E7 SCREEN / $07E8-$0800

!ifndef DEBUG {DEBUG=1}

!source "constants.inc"

; ZP
!addr CharCol=$02
!addr Tmp1=$07

*=$0801
!if DEBUG=0 {
    !byte $0b,$08,$b6,$07,$9e,$32,$30,$36,$31,$00,$00,$00   ; 1974 SYS2061
} else {
    !initmem $74
    !byte $0b,$08,$00,$13,$9e,$34,$38,$36,$34,$00,$00,$00   ; 4864 SYS4864 ; developer init
}

;2061
start:
            ; TODO move (most) VIC stuff to INIT
            cld
            lda #BLACK
            sta $D020
            lda #BROWN
            sta $D021

            ldy #<($0400+15*40+7)
            lda #>($0400+15*40+7)
            jsr SetCursor
            ldy #T_WASHERE
            jsr DrawText

-           jsr $FFE4 ; Get From Keyboard
            beq -
            rts


;----------------------------------------------------------------------------
; CARD DRAWING
;----------------------------------------------------------------------------
            *=$0900 ; DEBUG
; draws card template at cursor+X in selected color and moves cursor
DrawCard:
            lda #GREY
            sta CharCol
            ldx #5-1
-           lda Card_Frame_Plain_Top,x
            jsr PutChar
            dex
            bpl -
            jsr CursorDown
            ldy #4
-           ldx #0
            lda #Card_Frame_Left
            jsr PutChar
            ; TODO glyph + buffer color
            ldx #4
            lda #Card_Frame_Right
            jsr PutChar
            jsr CursorDown
            dey
            bne -
            ldx #5-1
-           lda Card_Frame_Plain_Bottom,x
            jsr PutChar
            dex
            bpl -
            rts


;----------------------------------------------------------------------------
; TEXT DRAWING
;----------------------------------------------------------------------------

; TODO: Switch X and Y around, so cursor can be put in ZP! This will save some bytes

; draws text Y at cursor+X (clobbers X,Tmp1)
DrawText:
            dex
-           lda TextData,y
            beq +++                     ; text ends with 0
            bpl .no_macro
            jsr DrawMacro
            jmp .put_space
.no_macro:
            jsr MovePutChar3F           ; C=1 if >=$40
            bcc +
.put_space:
            lda #32
            jsr MovePutChar
+           iny
            bne -
+++         rts

; draws macro A ($80..$FF) at cursor+X-1 (clobbers X,Tmp1)
DrawMacro:
            sty Tmp1
            tay                         ; A will be $80..$FF
--          lda TextMacroData-$80,y
            jsr MovePutChar3F           ; C=1 if >=$40
            bcs ++                      ; macros end with +$40
            iny
            bne --
++          ldy Tmp1
            rts


;----------------------------------------------------------------------------
; CHARACTER DRAWING
;----------------------------------------------------------------------------

MovePutChar3F:
            cmp #$40 ; C=1 if >=$40
            and #$3F
MovePutChar:
            inx
; draws char in A at cursor+X with color CharCol (clobbers X)
PutChar:
            .putc1=*+1
            .putc1h=*+2
            sta $0400,x
            pha
            lda CharCol
            .putc2=*+1
            .putc2h=*+2
            sta $d400,x
            pla
            rts

; puts cursor at Y/A Y=low byte, A=high byte (clobbers A)
SetCursor:
            sty .putc1
            sty .putc2
            sta .putc1h
            eor #$dc     ; turn 4/5/6/7 into $D8/9/a/b
            sta .putc2h
            rts

; moves the cursor a row down (clobbers A)
CursorDown:
            lda .putc1 ; low byte
            clc
            adc #40
            sta .putc1
            sta .putc2
            bcc +
            inc .putc1h
            inc .putc2h
+           rts


;----------------------------------------------------------------------------
; DATA
;----------------------------------------------------------------------------

Card_Frame_Plain_Top:
    !byte 79,119,119,119,80     ; plain top row
Card_Frame_Left = 116
Card_Frame_Right = 106
Card_Frame_Plain_Bottom:
    !byte 76,111,111,111,122    ; plain bottom row

; card layout:
; 5 chars wide, 6 high P---] |/\/| |/\/| |/\/| |   | AA-DD
; 79,119,119,119,80
; 116,0,0,0,106 x4 ;; ;HE? 101==116 en 106==103 WTF? zelfs in LOWER CASE?
; 76,111,111,111,122
; the glyph is 3x3 in the suit color
; each card has a name and some text associated with it
; if there's room we can even add flavor text

; 3x3 bytes per glyph
Glyphs:


;----------------------------------------------------------------------------
; TEXT
; Note that this can be max $180 bytes!
;----------------------------------------------------------------------------

; A text can point to macro text that comes from another block, thereby indexable
; char=0 -> end, chars<$40 -> put, chars>$80 -> macro lookup char-$80
TextData:
        T_WASHERE=*-TextData
        !scr TM_POLYSTYRENE,"waShere",0
!if *-TextData >= $FF { !error "Out of TextData memory" }

; Max offset is 127, so this is really LIMITED
TextMacroData:
        TM_POLYSTYRENE=$80+*-TextMacroData
        !scr "polystyren",'e'+$40
        TM_GOBLIN=$80+*-TextMacroData
        !scr "gobli",'n'+$40
        TM_CANDY=$80+*-TextMacroData
        !scr "cand",'y'+$40
        TM_SOAP=$80+*-TextMacroData
        !scr "soa",'p'+$40
!if *-TextMacroData >= $80 { !error "Out of TextMacroData memory" }

; Max 2K
!if * >= $1000 { !error "Out of memory" }
!fill $1000-*-1,$74
!byte $FF ; should be at $0FFF


;----------------------------------------------------------------------------
; INIT CODE (03D0-07FF)
;----------------------------------------------------------------------------
!if DEBUG=1 {
            ; copy loop for INIT at $1300=4864
            *=$1300
            ldx #$100-$D0-1
-           lda $13D0,x
            sta $03D0,x
            dex
            bpl -
            ldx #0
-           lda $1400,x
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
            *=$13D0 ; 5072
} else {
            *=$03D0
}
!pseudopc $03D0 {
INIT:
            ; set up ZP pointers
            jmp start
}
!fill $0800-(*&$0FFF)-1,$74
!byte $FF ; should be at $07FF
