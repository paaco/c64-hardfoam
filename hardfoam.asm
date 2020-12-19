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
!addr Tmp2=$08
!addr ZP_RNG_LOW = $09
!addr ZP_RNG_HIGH = $0A

*=$0801
!if DEBUG=1 {
    !initmem $74
    !byte $0b,$08,$00,$13,$9e,$34,$38,$36,$34,$00,$00,$00   ; 4864 SYS4864 ; developer init
}

; 2049 in DEBUG=0 mode, so use Exomizer to run from $0801
Start:
            ; upper player has 7 health
            ldx #<($0400+39)
            lda #>($0400+39)
            jsr SetCursor
            lda #10
            sec
            sbc #7
            tax
            lda #$0A
            ldy #0
            jsr DrawHealthBar

            ; lower player has 4 health
            ldx #<($0400+15*40+39)
            lda #>($0400+15*40+39)
            jsr SetCursor
            ldx #4
            lda #$A0
            ldy #0
            jsr DrawHealthBar

            ldx #<($0400+15*40+7)
            lda #>($0400+15*40+7)
            jsr SetCursor

            lda #GREY
            sta CharCol

            ldy #$FF
            ldx #T_WASHERE
            jsr DrawText

            ldx #<($0400+4*40)
            lda #>($0400+4*40)
            jsr SetCursor

            ldy #0
            jsr DrawCardBack

            ldy #10-2
            lda #%01000110
            jsr DrawCardTopFrameDecorated
            lda #$34
            jsr DrawCardBottomDecoration

-           jsr $FFE4 ; Get From Keyboard
            beq -
            jmp * ; prevents RUN/STOP to break


;----------------------------------------------------------------------------
; TEXT DRAWING
;----------------------------------------------------------------------------

; draws text X at cursor + Y+1 (clobbers A,X,Y,Tmp1)
DrawText:
-           lda TextData,x
            beq ++++                    ; text ends with 0
            bpl +
            jsr DrawMacro
            jmp ++
+           jsr MovePutChar3F           ; C=1 if >=$40
            bcc +++
++          lda TextData+1,x            ; lookahead
            beq ++++                    ; text ends directly with 0
            lda #32                     ; put space
            jsr MovePutChar
+++         inx
            bne -
++++        rts

; draws macro A ($80..$FF) at cursor + Y+1 (clobbers A,Y,Tmp1)
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

; draws block X at cursor + Y+1 (clobbers A,X,Y)
DrawBlock:
-           lda BlockData,x
            beq +
            jsr MovePutChar
            inx
            bne -
+           rts


;----------------------------------------------------------------------------
; CARD DRAWING
;----------------------------------------------------------------------------

; draws card background at cursor (clobbers A,X,Y,Tmp1,Tmp2)
DrawCardBack:
            ldx #B_CARDBACKTOP
            jsr DrawBlockAddModulo
            lda #4
            sta Tmp2
-           ldx #B_CARDBACKMIDDLE
            jsr DrawBlockAddModulo
            dec Tmp2
            bne -
            ldx #B_CARDBACKBOTTOM
            jmp DrawBlockAddModulo

; draws card top frame at cursor + Y+1 (clobbers A,X,Y,Tmp1,Tmp2)
DrawCardTopFrameDecorated:
            jsr DrawCardTopDecoration
            ldx #B_FRAMETOP+2
            bne .top_frame              ; always

; draws card top frame at cursor + Y+1 (clobbers A,X,Y,Tmp1,Tmp2)
DrawCardTopFrame:
            ldx #B_FRAMETOP
.top_frame: jsr DrawBlockAddModulo
            lda #3
            sta Tmp2
-           lda #116 ; Left side (petscii uses 116, but 101 is identical)
            jsr MovePutChar
            lda #3
            jsr AddAToY
            lda #106 ; Right side (petscii uses 106, but 103 is identical)
            jsr MovePutChar
            jsr AddFrameModuloToY
            dec Tmp2
            bne -
            ldx #B_FRAMEBOTTOM
            bne DrawBlockAddModulo      ; always

; draws card top frame LTSSCCCC decoration A at cursor + Y+1 (clobbers A,X,Y)
DrawCardTopDecoration:
            pha
            ldx #$D7                    ; T=0(spell)  D7 (ball)
            ;     LTSSCCCC
            and #%01000000
            beq +
            inx                         ; T=1(Monster) D8 (clover)
+           txa
            jsr MovePutChar
            pla
            ;     LTSSCCCC
            and #%00001111
            ora #$B0 ; 0..9 reversed
            bne MovePutChar             ; always

; draws card bottom A/D* decoration A at cursor + Y+1 (clobbers A,X,Y)
DrawCardBottomDecoration:
            pha
            pha
            lda #206
            jsr MovePutChar
            pla
            lsr
            lsr
            lsr
            lsr
            jsr +
            ldx #B_SPACEHEART
            jsr DrawBlock
            pla
            and #$0F
+           ora #$B0
            bne MovePutChar             ; always

;----------------------------------------------------------------------------
; CHARACTER DRAWING
;----------------------------------------------------------------------------

; draws char in A at cursor+Y+1 with color CharCol (clobbers Y) and sets C=1 if A>=$40
MovePutChar3F:
            cmp #$40 ; C=1 if >=$40
            and #$3F
; draws char in A at cursor+Y+1 with color CharCol (clobbers Y)
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

; puts cursor at X/A X=low byte, A=high byte (clobbers A)
SetCursor:
            stx _CursorPos
            stx _ColorPos
            sta _CursorPos+1
            eor #$DC                    ; turn 4/5/6/7 into $D8/9/a/b
            sta _ColorPos+1
            rts

; moves cursor down a row (clobbers A)
MoveCursorDown:
            lda _CursorPos
            clc
            adc #40
            sta _CursorPos
            sta _ColorPos
            bcc +
            inc _CursorPos+1
            inc _ColorPos+1
+           rts

; draws block X at cursor + Y+1 and adds 40-(width of card frame) to Y (clobbers A,Y,Tmp1)
DrawBlockAddModulo:
            jsr DrawBlock
; adds 40-(width of card frame) to Y (clobbers A,Y,Tmp1)
AddFrameModuloToY:
            lda #40-5
; adds A to Y (clobbers A,Y,Tmp1)
AddAToY:
            sta Tmp1
            tya
            clc
            adc Tmp1
            tay
            rts


;----------------------------------------------------------------------------
; HEALTH DRAWING
;----------------------------------------------------------------------------

; draws X(0..10) as 10 high health bar in colors in A (2 nibbles) at cursor + Y+1 (clobbers A,X,Tmp1)
DrawHealthBar:
            sta CharCol
            stx Tmp1
            ldx #10
-           cpx Tmp1
            bne +
            ; switch to second color
            lsr CharCol
            lsr CharCol
            lsr CharCol
            lsr CharCol
+           lda #$53 ; heart
            jsr PutChar
            jsr MoveCursorDown
            dex
            bne -
            rts


;----------------------------------------------------------------------------
; prng
;----------------------------------------------------------------------------

; RANDOM routine from https://codebase64.org/doku.php?id=base:16bit_xorshift_random_generator
; the RNG. You can get 8-bit random numbers in A or 16-bit numbers
; from the zero page addresses. Leaves X/Y unchanged.
random:
        LDA ZP_RNG_HIGH
        LSR
        LDA ZP_RNG_LOW
        ROR
        EOR ZP_RNG_HIGH
        STA ZP_RNG_HIGH ; high part of x ^= x << 7 done
        ROR             ; A has now x >> 9 and high bit comes from low byte
        EOR ZP_RNG_LOW
        STA ZP_RNG_LOW  ; x ^= x >> 9 and the low part of x ^= x << 7 done
        EOR ZP_RNG_HIGH
        STA ZP_RNG_HIGH ; x ^= x << 8 done
        RTS


;----------------------------------------------------------------------------
; CARDS SoA
;----------------------------------------------------------------------------

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
; GLYPHS
;----------------------------------------------------------------------------

; 8*4 glyphs * 9 bytes = 288 bytes; 28 glyphs would be 28*9=252
GlyphData:
    G_LEGND_GOBLIN=*-GlyphData
    !byte 73,104,85, 215,215,117, 81,73,41


;----------------------------------------------------------------------------
; TEXT
;----------------------------------------------------------------------------

; Char=0->End, Char<$40->Put, Chars<$80->Put+Space, Chars>=$80->Macro Lookup Char-$80 (Max 255 bytes)
TextData:
    T_WASHERE=*-TextData
    !scr TM_POLYSTYRENE,"waShere",0
!if *-TextData >= $FF { !error "Out of TextData memory" }

; Macros (Max 128 bytes) Max offset is 127, so this is really LIMITED
TextMacroData:
    TM_POLYSTYRENE=$80+*-TextMacroData
    !scr "polystyrenE"
    TM_GOBLIN=$80+*-TextMacroData
    !scr "gobliN"
    TM_CANDY=$80+*-TextMacroData
    !scr "candY"
    TM_SOAP=$80+*-TextMacroData
    !scr "soaP"
    TM_LEGENDARY=$80+*-TextMacroData
    !scr "legendarY"
!if *-TextMacroData >= $80 { !error "Out of TextMacroData memory" }

; Plain text blocks (Max 255 bytes)
BlockData:
    B_FRAMETOP=*-BlockData
    !byte 79,119,119,119,80,0
    B_FRAMEBOTTOM=*-BlockData
    !byte 76,111,111,111,122,0
    B_SPACEHEART=*-BlockData
    !byte 160,211,0
    B_CARDBACKTOP=*-BlockData
    !byte 236,192,192,192,251,0
    B_CARDBACKMIDDLE=*-BlockData
    !byte 194,102,102,102,194,0
    B_CARDBACKBOTTOM=*-BlockData
    !byte 252,192,192,192,254,0
!if *-BlockData >= $FF { !error "Out of BlockData memory" }

SIZEOF_TEXT=*-TextData

;----------------------------------------------------------------------------
; MAX 2K ALLOWED HERE
;----------------------------------------------------------------------------
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

            jmp Start
}
!fill $0800-(*&$0FFF),$20
