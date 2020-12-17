; Hard Foam - a 2K card game
; Developed for the https://itch.io/jam/the-c64-cassette-50-charity-competition

; Only WRITES memory < $1000 and uses Dxxx IO, KERNAL and BASIC calls

; Note that it is only required to load below $1000, not specifically $0801,
; so we could even load at $0400 (not lower to keep Tape loading compatibility)
; However, loading it there (anything below $0801) will kill RUN, only allow direct SYS
; Exomizer also uses $0334-$03D0 as decrunching buffer; decrunching there will hang
; AND: we may only WRITE <$1000, so reading BASIC/KERNAL would be OK

; Usable RAM:  $0200-$0258 (89 bytes) / $0293-$02FF (109 bytes)
; Usable INIT: $03D0-$03FF (48 bytes) / $0400-$07E7 SCREEN (200 bytes 5 middle rows) / $07E8-$0800 (24 bytes)

INIT=$0400 ; use this as run address for cruncher

!ifndef DEBUG {DEBUG=1}

!source "constants.inc"

; ZP addresses
!addr CharCol=$02
!addr _CursorPos=$03 ; ptr
!addr _ColorPos=$05 ; ptr
!addr Tmp1=$07

*=$0801
!if DEBUG=1 {
    !initmem $74
    !byte $0b,$08,$00,$13,$9e,$34,$38,$36,$34,$00,$00,$00   ; 4864 SYS4864 ; developer init
}

; 2049 in DEBUG=0 mode, so use Exomizer to run from $0801
start:
            ldy #<($0400+15*40+7)
            lda #>($0400+15*40+7)
            jsr SetCursor
            ldy #0
            ldx #T_WASHERE
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

; draws text X at cursor+Y (clobbers X,Y,Tmp1)
DrawText:
            dey
-           lda TextData,x
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
+           inx
            bne -
+++         rts

; draws macro A ($80..$FF) at cursor+Y-1 (clobbers X,Tmp1)
DrawMacro:
            stx Tmp1
            tax                         ; A will be $80..$FF
--          lda TextMacroData-$80,x
            jsr MovePutChar3F           ; C=1 if >=$40
            bcs ++                      ; macros end with +$40
            inx
            bne --
++          ldx Tmp1
            rts


;----------------------------------------------------------------------------
; CHARACTER DRAWING
;----------------------------------------------------------------------------

MovePutChar3F:
            cmp #$40 ; C=1 if >=$40
            and #$3F
MovePutChar:
            iny
; draws char in A at cursor+Y with color CharCol
PutChar:
            sta (_CursorPos),y
            pha
            lda CharCol
            sta (_ColorPos),y
            pla
            rts

; puts cursor at Y/A Y=low byte, A=high byte (clobbers A)
SetCursor:
            sty _CursorPos
            sty _ColorPos
            sta _CursorPos+1
            eor #$dc     ; turn 4/5/6/7 into $D8/9/a/b
            sta _ColorPos+1
            rts

; moves the cursor a row down (clobbers A)
CursorDown:
            lda _CursorPos ; low byte
            clc
            adc #40
            sta _CursorPos
            sta _ColorPos
            bcc +
            inc _CursorPos+1
            inc _ColorPos+1
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
; CARDS
;----------------------------------------------------------------------------

; Cards Data (SoA)
; 1 byte LTSSCCCC : L=Legendary T=Type(0=Monster/1=Spell) SS=Suit(0,1,2,3) CCCC=Cost(0..15)
Cards_LTSuitCost:
    !byte 0
; 1 byte AAAADDDD : AAAA=Attack DDDD=Defense
Cards_AttackDefense:
    !byte 0
; 1 byte Name TextPtr
Cards_Name:
    !byte 0
; 1 byte Effect TextPtr (also used to perform effect)
Cards_EffectText:
    !byte 0
; 1 byte GlyphPtr
Cards_Glyph:
    !byte 0


;----------------------------------------------------------------------------
; TEXT
; Note that this can be max $180 bytes!
;----------------------------------------------------------------------------

; Char=0->End, Char<$40->Put, Chars<$80->Put+Space, Chars>=$80->Macro Lookup Char-$80 (Max 256 bytes)
TextData:
    T_WASHERE=*-TextData
    !scr TM_POLYSTYRENE,"waShere",0
!if *-TextData >= $FF { !error "Out of TextData memory" }

; Macros (Max 128 bytes) Max offset is 127, so this is really LIMITED
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


;----------------------------------------------------------------------------
; DEBUG BUILD ONLY copy loop for INIT at $1300=4864
;----------------------------------------------------------------------------
!if DEBUG=1 {
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
            jmp REALINIT
            *=$13D0 ; 5072
} else {
            *=$03D0
}

;----------------------------------------------------------------------------
; DATA (03D0-03FF)
;----------------------------------------------------------------------------
            !fill $30,0 ; FREE RAM AT $03D0-$0400

;----------------------------------------------------------------------------
; INIT CODE (0400-07FF)
;----------------------------------------------------------------------------
!pseudopc $0400 {
            ; TODO add PETSCII logo here in the upper rows?
            ; INIT CODE
REALINIT:
!if INIT != REALINIT { !error "REALINIT=", REALINIT, " so update INIT and make file!" }
            cld ; who knows?

            ; setup VIC
            lda #BLACK
            sta $D020
            lda #BROWN
            sta $D021
            lda #20 ; default uppercase
            sta $D018
            ; lock uppercase
            lda #$80
            sta $0291

            jmp start
}
!fill $0800-(*&$0FFF),$20
