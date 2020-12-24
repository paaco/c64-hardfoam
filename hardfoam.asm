; HARD FOAM - a 2K card game
; Developed for the https://itch.io/jam/the-c64-cassette-50-charity-competition

; Only WRITES memory < $1000 and uses Dxxx IO, calling/reading KERNAL/BASIC is OK

; Note that it is only required to load below $1000, not specifically $0801,
; so we could even load at $0400 (not lower to keep Tape loading compatibility)
; However, loading it there (anything below $0801) will kill RUN, only allow direct SYS
; Exomizer also uses $0334-$03D0 as decrunching buffer; decrunching there will hang

; Without packer it's possible to load and run $120-$1000 giving 3808 bytes instead of 2047 packed
; Set IRQ to $EA81 to keep memory safe from overwriting.
; Holes at $1ED-$0200 and $0314-$032A

; Usable RAM:  $0120-$0276 (343 bytes) (with stack reduced to $20) / $0293-$02FF (109 bytes)
; Note         $01ED-$200 (19 bytes) are squashed during loading
; Usable RAM:  $0200-$03FF (512 bytes)
; Note         $0314-$032A (22 bytes) are required vectors into ROM
; Usable INIT: $03D0-$03FF (48 bytes) / $0400-$07E7 SCREEN (200 bytes 5 middle rows) / $07E8-$0800 (24 bytes)

; Approx 0c3f-0801 = 1086 bytes code and 0d23-0c3f bytes data = 228 bytes, total 1314 crunched to 1384 bytes from $0801

INIT=$0400 ; use this as run address for cruncher

!ifndef DEBUG {DEBUG=1}

;!source "constants.inc" ; older acme doesn't support same-dir includes
BLACK=0
WHITE=1
GREEN=5
BLUE=6
YELLOW=7
ORANGE=8
LIGHT_RED=10
GREY=12
; colors
COL_BORDER=BLACK
COL_SCREEN=BLUE
COL_HEALTH_ON=LIGHT_RED
COL_HEALTH_OFF=BLACK
COL_PLAIN=GREY
COL_LEGEND=YELLOW
COL_DISABLED=ORANGE
COL_SELECTED=WHITE
COL_HIGHER=GREEN
COL_LOWER=LIGHT_RED
CHR_SPACE=32+DEBUG*10 ; space or star

; ZP addresses
!addr CharCol=$02
!addr _CursorPos=$03 ; ptr
!addr _ColorPos=$05 ; ptr
!addr Tmp1=$07
!addr Tmp2=$08
!addr Tmp3=$09
!addr Tmp4=$0A
!addr ZP_RNG_LOW = $0B
!addr ZP_RNG_HIGH = $0C
!addr Suit=$0D
!addr CardIdx=$0E
!addr TableIdx=$0F
; player data (consecutive)
!addr PlayerData=$10
    PD_LIFE=0       ; 0 .. 10
    PD_ENERGY=1     ; $30 .. $39
    ; fixed '/' character in between
    PD_MAXENERGY=3  ; $30 .. $39
    PD_REMAIN=4     ; DECKSIZE-1 .. 0
    PD_HAND=5       ; 7 bytes (card#), >=$80=no card
    PD_TABLE=12     ; 20 bytes 5*4 bytes (card#,atd,def,status) TODO: SoA
      TD_CARD=0     ; card# >=$80=no card
      TD_ATK=1      ; attack
      TD_DEF=2      ; defense
      TD_STATUS=3   ; status: 0=normal, 1=tapped, >=$80 selected
      SIZEOF_TD=4
    PD_DECK=32      ; 32 bytes (card#)
SIZEOF_PD=64
!addr AIData=$10+SIZEOF_PD
; ZP gets a bit wiffy from $A0 so be careful

*=$0801
!if DEBUG=1 {
    !initmem $74
    !byte $0b,$08,$00,$13,$9e,$34,$38,$36,$34,$00,$00,$00   ; 4864 SYS4864 ; developer init
}

; 2049 in DEBUG=0 mode, so use Exomizer to run from $0801
Start:
            jsr InitPlayersData

            jsr ScreenPickLeader

            jmp MainLoop

            jsr DrawHealthBars
            jsr DrawCounters

            lda #GREY
            sta CharCol
            jsr DrawStackSides
            jsr ClearUpperLines
            jsr ClearLowerLines

            ldx #<($0400+4*40)
            lda #>($0400+4*40)
            jsr SetCursorY0
            jsr DrawCardBack

            ldx #<($0400+15*40)
            lda #>($0400+15*40)
            jsr SetCursorY0
            lda #5 ; card# (goes per 5)
            sta CardIdx
            jsr DrawCard

            ldx #<($0400+15*40+8)
            lda #>($0400+15*40+8)
            jsr SetCursorY0
            ldx #PlayerData+PD_TABLE ; zP offset into table (increases per 4)
            ; setup card on table
            lda #0 ; card# (goes per 5)
            sta TD_CARD,x
            lda #$81 ; tapped=1,selected=$80
            sta TD_STATUS,x
            lda #3
            sta TD_ATK,x
            lda #1
            sta TD_DEF,x
            ; setup card on table
            lda #5 ; card# (goes per 5)
            sta TD_CARD+4,x
            ;sta TD_CARD+8,x
            sta TD_CARD+12,x
            sta TD_CARD+16,x
            lda #$00 ; tapped=1,selected=$80
            sta TD_STATUS+4,x
            lda #2
            sta TD_ATK+4,x
            lda #2
            sta TD_DEF+4,x
            jsr DrawTable

            ldx #<($0400+4*40+8)
            lda #>($0400+4*40+8)
            jsr SetCursorY0
            ldx #PlayerData+PD_TABLE ; zP offset into table (increases per 4)
            jsr DrawTable
            ldy #6*4
            jsr DrawCardSelect
            ldy #0
            lda #BLACK
            sta CharCol
            lda #102
            jsr FillCard

MainLoop:
-           jsr $FFE4 ; Get From Keyboard : $9D=LEFT,$1D=RIGHT, $91=UP,$11=DOWN, $0D=ENTER, $20=SPACE, $03=R/S
            beq -
            sta $0427
            jmp MainLoop


;----------------------------------------------------------------------------
; GAME SCREENS
;----------------------------------------------------------------------------

ScreenPickLeader:
            jsr ClearAll

            lda #GREY
            sta CharCol
            ldx #<($0400+1*40+9)
            lda #>($0400+1*40+9)
            jsr SetCursorY0
            lda #T_PICK_LEADER
            jsr DrawText

            ; Draw decorated leaders in upper part
            ldx #<($0400+2*40+8)
            lda #>($0400+2*40+8)
            jsr SetCursorY0
            ldx #0
-           stx TableIdx
            lda SuitLeaders,x
            sta CardIdx
            jsr DrawCard
            ldx TableIdx
            inx
            cpx #4
            bne -

            lda #1                      ; start somewhere
            sta TableIdx

.redrawleader:
            lda TableIdx
            tax
            asl
            asl
            clc
            adc TableIdx
            adc TableIdx                ; A=TableIdx*6
            sta Tmp3
            lda SuitLeaders,x
            sta CardIdx

            ; show selected card name and effect
            ldx #<($0400+8*40+8)
            lda #>($0400+8*40+8)
            jsr SetCursorY0
            ; wipe previous text
            lda #CHR_SPACE
-           sta (_CursorPos),y
            iny
            cpy #72
            bne -
            ldy #0
            jsr DrawCardText

            ; Create selectable deck cards in temporary table
            ldx #6-1
            lda CardIdx
-           clc
            adc #SIZEOF_CARD
            sta PlayerData+PD_HAND,x
            dex
            bpl -

            ; Draw deck cards in lower part
            ldx #<($0400+15*40+2)
            lda #>($0400+15*40+2)
            jsr SetCursorY0
            lda #6-1
            sta Tmp4
-           ldx Tmp4
            lda PlayerData+PD_HAND,x
            sta CardIdx
            jsr DrawCard
            dec Tmp4
            bpl -

            ldx #<($0400+2*40+8)
            lda #>($0400+2*40+8)
            jsr SetCursorY0
            ldy Tmp3
            jsr DrawCardSelect

            ; TODO: Move left/right/OK

            ldx #10 ; delay a lot
--          lda #$84
-           cmp $d012
            bne -
            inc $d020
-           cmp $d012
            beq -
            dec $d020
            dex
            bne --

            ldy Tmp3
            jsr ClearCardSelect

            inc TableIdx
            lda TableIdx
            and #$03
            sta TableIdx

            jmp .redrawleader


;----------------------------------------------------------------------------
; GAME FUNCTIONS
;----------------------------------------------------------------------------

InitPlayersData:
            ldx #SIZEOF_PD-1
-           lda #$FF
            sta PlayerData,x
            sta AIData,x
            dex
            cpx #SIZEOF_INITDATA-1
            bne -
-           lda InitData,x
            sta PlayerData,x
            sta AIData,x
            dex
            bpl -
            rts


;----------------------------------------------------------------------------
; UI DRAWING
;----------------------------------------------------------------------------

; draws 2 lines left of the card backs to simulate the stack (this stays the same during the game)
DrawStackSides:
            ldx #<($0400+3*40)
            lda #>($0400+3*40)
            jsr SetCursorY0
            lda #112
            jsr .put2
            lda #CHR_SPACE
            jsr PutCharMoveDown
            ldx #<($0400+15*40)
            lda #>($0400+15*40)
            jsr SetCursorY0
            lda #CHR_SPACE
            jsr .put2
            lda #109
            jmp PutCharMoveDown
            ; common logic
.put2:      jsr PutCharMoveDown
            ldx #5
-           lda #66
            jsr PutCharMoveDown
            dex
            bne -
            rts

; clears the two lower lines - starts the upper with 4 line chars (clobbers A,X,Y)
ClearLowerLines:
            ldx #<($0400+21*40)
            lda #>($0400+21*40)
            jsr SetCursorY0
            jsr .lines
            ldx #0
            jmp .spaces

; clears two lines upper lines - starts the lower with 4 line chars (clobbers A,X,Y)
ClearUpperLines:
            ldx #<($0400+2*40)
            lda #>($0400+2*40)
            jsr SetCursorY0
            ldx #0
            jsr .spaces
            ; fall through
.lines:     ldx #0
-           lda #67 ; line
            jsr MovePutChar
            inx
            cpx #4
            bne -
.spaces:    lda #CHR_SPACE
            jsr MovePutChar
            inx
            cpx #38
            bne .spaces
            iny
            iny
            rts

; draws both health bars (clobbers A,X,Y,Tmp1,cursor)
DrawHealthBars:
            ; upper player
            ldx #<($0400+39)
            lda #>($0400+39)
            jsr SetCursorY0
            lda #10
            sec
            sbc AIData+PD_LIFE
            tax
            lda #COL_HEALTH_OFF * 16 + COL_HEALTH_ON
            jsr .healthbar
            ; lower player
            ldx #<($0400+15*40+39)
            lda #>($0400+15*40+39)
            jsr SetCursorY0
            ldx PlayerData+PD_LIFE
            lda #COL_HEALTH_ON * 16 + COL_HEALTH_OFF
            ; fall through

; draws X(0..10) as 10 high health bar in colors in A (2 nibbles) at cursor + Y (clobbers A,X,Tmp1,cursor)
.healthbar:
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
            jsr PutCharMoveDown
            dex
            bne -
            rts

; draws energy and deck counters (clobbers A,X,Y)
DrawCounters:
            ; energy
            ldx #2
-           lda AIData+PD_ENERGY,x
            sta $0400,x
            lda PlayerData+PD_ENERGY,x
            sta $0400+24*40,x
            dex
            bpl -
            ; deck counters
            lda AIData+PD_REMAIN
            jsr AtoASCII2
            stx $0400+1*40
            sta $0400+1*40+1
            lda PlayerData+PD_REMAIN
            jsr AtoASCII2
            stx $0400+23*40
            sta $0400+23*40+1
            rts

; Converts .A to 3 ASCII/PETSCII digits: .Y = hundreds, .X = tens, .A = ones
AtoASCII2:
            ; ldy #$2f
            ldx #$3a
            sec
-           ; iny
            sbc #100
            ; bcs -
-           dex
            adc #10
            bmi -
            adc #$2f
            ; <10 "9 "
            cpx #$30
            bne +
            tax
            lda #CHR_SPACE
+           rts

; wipes the top and bottom of the screen completely (clobbers A,X)
ClearAll:
            ldx #0
            lda #CHR_SPACE
-           sta $0400,x
            sta $0400+5*40,x
            sta $0400+15*40,x
            sta $0400+20*40,x
            inx
            cpx #5*40
            bne -
            rts

; draws card selection symbols around a card at cursor + Y+1 (clobbers A,Y,Tmp1)
; - cursor + Y+1 is assumed to be top-left of card
DrawCardSelect:
            lda #COL_SELECTED
            sta CharCol
            lda #2*40
            jsr AddAToY
            lda #'>'
            jsr PutChar
            lda #6
            jsr AddAToY
            lda #'<'
            jmp PutChar

; clears card selection symbols around a card at cursor + Y+1 (clobbers A,Y,Tmp1)
; - cursor + Y+1 is assumed to be top-left of card
ClearCardSelect:
            lda #2*40
            jsr AddAToY
            lda #CHR_SPACE
            jsr PutChar
            lda #6
            jsr AddAToY
            lda #CHR_SPACE
            jmp PutChar


;----------------------------------------------------------------------------
; BASIC DRAWING
;----------------------------------------------------------------------------

; draws text A at cursor + Y+1 (clobbers A,X,Y,Tmp1)
DrawText:
            tax
-           lda TextData,x
            beq +                       ; text ends with 0
            jsr DrawMacro
            lda TextData+1,x            ; lookahead
            beq +                       ; text ends directly with 0
            lda #CHR_SPACE
            jsr MovePutChar
            inx
            bne -
+           rts

; draws macro A (1..255) at cursor + Y+1 (clobbers A,Y,Tmp1)
DrawMacro:
            stx Tmp1
            cmp #M_SUIT
            bne +
            ldx Suit
            lda SuitTextData,x
+           tax
-           lda MacroData-1,x
            bpl ++
            and #$7F                    ; last character
            ldx #$FF                    ; ends loop
++          jsr MovePutChar
            inx
            bne -
            ldx Tmp1
            rts

; draws block X at cursor + Y+1 (clobbers A,X,Y)
DrawBlock:
-           lda BlockData,x
            beq +
            jsr MovePutChar
            inx
            bne -
+           rts

; draws glyph X at cursor + Y+1 (clobbers A,X,Y,Tmp2)
DrawGlyph:
            lda #3
            sta Tmp2
-           lda GlyphData1,x
            jsr MovePutChar
            lda #40
            jsr AddAToY
            lda GlyphData2,x
            jsr PutChar
            lda #40
            jsr AddAToY
            lda GlyphData3,x
            jsr PutChar
            lda #256-80 ; return to top of glyph
            jsr AddAToY
            inx
            dec Tmp2
            bne -
            rts


;----------------------------------------------------------------------------
; CARD DRAWING
;----------------------------------------------------------------------------

; clears the size of a card at cursor + Y+1 (clobbers A,X,Y,Tmp1)
ClearCard:
            lda #CHR_SPACE
; fills the size of a card with A at cursor + Y+1 (clobbers A,X,Y,Tmp1)
FillCard:
            ldx #6
            stx Tmp2
--          ldx #5
-           jsr MovePutChar
            dex
            bne -
            pha
            jsr AddFrameModuloToY
            pla
            dec Tmp2
            bne --
            lda #256-40*6+6                 ; fixup position
            jmp AddAToY

; draws table in X at cursor + Y+1 (clobbers A,X,Y,Tmp1,Tmp2,Tmp3,CardIdx,TableIdx)
DrawTable:
            lda #5
            sta Tmp3
            stx TableIdx
-           jsr DrawTableCard
            lda TableIdx
            clc
            adc #4
            sta TableIdx
            dec Tmp3
            bne -
.stealrts1: rts

; draws card on table in TableIdx at cursor + Y+1 (clobbers A,X,Y,Tmp1,Tmp2,CardIdx)
; - draws status border, no decoration and highlighted attack/defense
DrawTableCard:
            ldx TableIdx
            lda TD_CARD,x
            cmp #$FF
            beq ClearCard
+           sta CardIdx
            tax
            lda Cards+CARD_LTSC,x
            jsr SetFrameCharCol
            lda #<PutChar               ; enable color write
            sta .drawvaluefixup1
            ; use disabled color when tapped
            ldx TableIdx
            lda TD_STATUS,x             ; 0=normal, $1=tapped, >=$80 selected
            beq +
            bmi .selected
            lda #COL_DISABLED
            bne ++                      ; always
.selected:  lda #COL_SELECTED
++          sta CharCol
            lda #<PutCharNoColor        ; disable color write
            sta .drawvaluefixup1
+           jsr DrawCardTopFrame
            jsr .drawcard1              ; draw remainder of card
            ; overwrite card values with actuals
            lda #40*5-4
            jsr AddAToY
            ; update attack value
            ldx TableIdx
            jsr .drawvalue
            ; update defense value
            inx ; CAREFUL: THIS ASSUMES DEFENSE COMES DIRECTLY AFTER ATTACK IN MEMORY
            iny
            iny
            iny
            jsr .drawvalue
            lda #256-40*5+1             ; fixup position
            jmp AddAToY
; common code to read screen to determine high/low/same, X=table index
.drawvalue:
            lda (_CursorPos),y
            and #$0F
            cmp TD_ATK,x                ; CARDATK-ACTUALATK Z=1:equal, C=1 actual<card C=0 actual>card
            beq .stealrts1              ; done
            bcc .higher
            lda #COL_LOWER
            bne +                       ; always
.higher:    lda #COL_HIGHER
+           sta CharCol
            lda TD_ATK,x
            ora #$30                    ; regular digits
            .drawvaluefixup1=*+1        ; make jsr switchable between PutChar and PutCharNoColor
            jmp PutChar                 ; SELF-MODIFIED

; draws decorated card in CardIdx at cursor + Y+1 (clobbers A,X,Y,Tmp1,Tmp2)
; - draws legend border, full decoration and default attack/defense
DrawCard:
            ldx CardIdx
            lda Cards+CARD_LTSC,x
            jsr SetFrameCharCol
            ldx CardIdx
            lda Cards+CARD_LTSC,x
            jsr DrawCardTopFrameDecorated
.drawcard1: ldx CardIdx
            lda Cards+CARD_ATDF,x               ; card values not actuals
            jsr DrawCardBottomDecoration
            lda #92                             ; move to glyph position
            jsr AddAToY
            ldx CardIdx
            lda Cards+CARD_LTSC,x
            jsr SetSuitCharCol
            lda Cards+CARD_GLYPH,x
            tax
            jsr DrawGlyph
            lda #256-40+2                       ; fixup position
            jmp AddAToY

; draws text of card in CardIdx at cursor + Y+1 (clobbers A,X,Y,Tmp1,Tmp2)
DrawCardText:
            sty Tmp2
            ldx CardIdx
            lda Cards+CARD_LTSC,x
            jsr SetSuitCharCol
            lda Cards+CARD_NAME,x
            jsr DrawText
            ldy Tmp2
            lda #40
            jsr AddAToY
            ldx CardIdx
            lda Cards+CARD_EFFECT,x
            jmp DrawText

; draws card background at cursor + Y+1 (clobbers A,X,Y,Tmp1,Tmp2)
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
            jsr DrawBlockAddModulo
            lda #256-40*6+6             ; fixup position
            jmp AddAToY

; draws card top frame with decoration in A at cursor + Y+1 (clobbers A,X,Y,Tmp1,Tmp2)
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
+           ora #$B0                    ; reversed digit
            ; fall through to MovePutChar

;----------------------------------------------------------------------------
; CHARACTER DRAWING
;----------------------------------------------------------------------------

;!fill 6,$EA ; fixup when necessary

; draws char in A at cursor+Y+1 with color CharCol (clobbers Y)
MovePutChar:
            iny
; draws char in A at cursor+Y with color CharCol
PutChar:
            pha
            lda CharCol
            sta (_ColorPos),y
            pla
; draws char in A at cursor+Y
PutCharNoColor:
            sta (_CursorPos),y
            rts
!if (>PutChar != >PutCharNoColor) { !error "PutChar and PutCharNoColor should be in same page" }

; puts cursor at X/A X=low byte, A=high byte and sets Y=0 (clobbers A,Y)
SetCursorY0:
            ldy #0
            stx _CursorPos
            stx _ColorPos
            sta _CursorPos+1
            eor #$DC                    ; turn 4/5/6/7 into $D8/9/a/b
            sta _ColorPos+1
            rts

; moves cursor down and draws char in A at cursor+Y with color CharCol (clobbers A)
PutCharMoveDown:
            jsr PutChar
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

; draws graphic X at cursor + Y+1 and adds 40-(width of card frame) to Y (clobbers A,Y,Tmp1)
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

; convert LTSC value in A to frame color (legend/plain) and sets CharCol (clobber A)
SetFrameCharCol:
            and #$80
            beq + ; plain
            lda #COL_LEGEND
            bne ++
+           lda #COL_PLAIN
++          sta CharCol
            rts

; convert LTSC value in A to suit color and sets CharCol (clobbers A)
SetSuitCharCol:
            lsr
            lsr
            lsr
            lsr
            and #$03
            sta CharCol
            rts

;----------------------------------------------------------------------------
; prng
;----------------------------------------------------------------------------

; RANDOM routine from https://codebase64.org/ 16bit eor shift random generator
; the RNG. You can get 8-bit random numbers in A or 16-bit numbers
; from the zero page addresses. Leaves X/Y unchanged.
random:
        lda ZP_RNG_HIGH
        lsr
        lda ZP_RNG_LOW
        ror
        eor ZP_RNG_HIGH
        sta ZP_RNG_HIGH ; high part of x ^= x << 7 done
        ror             ; A has now x >> 9 and high bit comes from low byte
        eor ZP_RNG_LOW
        sta ZP_RNG_LOW  ; x ^= x >> 9 and the low part of x ^= x << 7 done
        eor ZP_RNG_HIGH
        sta ZP_RNG_HIGH ; x ^= x << 8 done
        rts


;----------------------------------------------------------------------------
; CARDS
;----------------------------------------------------------------------------

CARD_LTSC=0         ; 1 byte LTSSCCCC : L=Legendary T=Type(0=Spell/1=Monster) SS=Suit(0,1,2,3) CCCC=Cost(0..15)
CARD_ATDF=1         ; 1 byte AAAADDDD : AAAA=Attack DDDD=Defense
CARD_NAME=2         ; 1 byte Name TextPtr
CARD_EFFECT=3       ; 1 byte Effect TextPtr (also used to perform effect)
CARD_GLYPH=4        ; 1 byte GlyphPtr
SIZEOF_CARD=5

Cards:
    C_GOBLIN_LEADER=*-Cards
    !byte $C3, $32, N_GOBLIN_LEADER, E_ALL_GAIN11, G_LEGND_GOBLIN
    !byte $41, $12, N_WANNABE,       T_NONE,       G_WANNABE
    !byte $42, $12, N_WANNABE,       T_NONE,       G_WANNABE
    !byte $43, $12, N_WANNABE,       T_NONE,       G_WANNABE
    !byte $44, $12, N_WANNABE,       T_NONE,       G_WANNABE
    !byte $45, $12, N_WANNABE,       T_NONE,       G_WANNABE
    !byte $46, $12, N_WANNABE,       T_NONE,       G_WANNABE
    C_POLY_LEADER=*-Cards
    !byte $D3, $17, N_POLY_LEADER,   T_NONE,       G_LEGND_POLY
    !byte $51, $12, N_WANNABE,       T_NONE,       G_WANNABE
    !byte $52, $12, N_WANNABE,       T_NONE,       G_WANNABE
    !byte $53, $12, N_WANNABE,       T_NONE,       G_WANNABE
    !byte $54, $12, N_WANNABE,       T_NONE,       G_WANNABE
    !byte $55, $12, N_WANNABE,       T_NONE,       G_WANNABE
    !byte $56, $12, N_WANNABE,       T_NONE,       G_WANNABE
    C_CANDY_LEADER=*-Cards
    !byte $E0, $00, N_CANDY_LEADER,  T_NONE,       G_LEGND_CANDY
    !byte $61, $12, N_WANNABE,       T_NONE,       G_WANNABE
    !byte $62, $12, N_WANNABE,       T_NONE,       G_WANNABE
    !byte $63, $12, N_WANNABE,       T_NONE,       G_WANNABE
    !byte $64, $12, N_WANNABE,       T_NONE,       G_WANNABE
    !byte $65, $12, N_WANNABE,       T_NONE,       G_WANNABE
    !byte $66, $12, N_WANNABE,       T_NONE,       G_WANNABE
    C_SOAP_LEADER=*-Cards
    !byte $F0, $00, N_SOAP_LEADER,   T_NONE,       G_LEGND_SOAP
    !byte $71, $12, N_WANNABE,       T_NONE,       G_WANNABE
    !byte $72, $12, N_WANNABE,       T_NONE,       G_WANNABE
    !byte $73, $12, N_WANNABE,       T_NONE,       G_WANNABE
    !byte $74, $12, N_WANNABE,       T_NONE,       G_WANNABE
    !byte $75, $12, N_WANNABE,       T_NONE,       G_WANNABE
    !byte $76, $12, N_WANNABE,       T_NONE,       G_WANNABE
SIZEOF_CARDS=*-Cards
NUM_CARDS=SIZEOF_CARDS/5

;----------------------------------------------------------------------------
; GLYPHS
;----------------------------------------------------------------------------

; glyphs, 3 bytes per row = max 252/3 = 84 glyphs
GlyphData1:
    G_LEGND_GOBLIN=*-GlyphData1
    !byte 73,104,85
    G_LEGND_POLY=*-GlyphData1
    !byte 206,160,205
    G_LEGND_CANDY=*-GlyphData1
    !byte 223,98,233
    G_LEGND_SOAP=*-GlyphData1
    !byte 255,251,252
    G_WANNABE=*-GlyphData1
    !byte 127,98,126
GlyphData2:
    !byte 215,215,117
    !byte 160,182,160
    !byte 160,160,160
    !byte 160,160,160
    !byte 17,17,97
GlyphData3:
    !byte 81,73,41
    !byte 192,159,192
    !byte 105,226,95
    !byte 124,226,126
    !byte 124,251,78


;----------------------------------------------------------------------------
; TEXT
;----------------------------------------------------------------------------

; Each string is a list of MacroPtrs and ends with 0
TextData:
    N_GOBLIN_LEADER=*-TextData
    !byte M_GOBLIN,M_LEADER
    T_NONE=*-TextData
    !byte 0
    N_POLY_LEADER=*-TextData
    !byte M_POLYSTYRENE,M_BLOCK,0
    N_CANDY_LEADER=*-TextData
    !byte M_HARD,M_CANDY,0
    N_SOAP_LEADER=*-TextData
    !byte M_FOREVER,M_SOAP,0
    N_WANNABE=*-TextData
    !byte M_SUIT,M_WANNABE,0
    E_ALL_GAIN11=*-TextData
    !byte M_ALL,M_SUIT,M_GAIN11,0
    T_PICK_LEADER=*-TextData
    !byte M_PICK,M_LEGENDARY,M_LEADER,0
!if *-TextData >= $FF { !error "Out of TextData memory" }

; Text macros, each ends with a byte >= $80
MacroData:
    M_SUIT=2 ; is replaced by current suit name
    M_GOBLIN      =*-MacroData+1 : !scr "gobli",'n'+$80
    M_POLYSTYRENE =*-MacroData+1 : !scr "polystyren",'e'+$80
    M_CANDY       =*-MacroData+1 : !scr "cand",'y'+$80
    M_SOAP        =*-MacroData+1 : !scr "soa",'p'+$80
    M_HARD        =*-MacroData+1 : !scr "har",'d'+$80
    M_LEGENDARY   =*-MacroData+1 : !scr "legendar",'y'+$80
    M_LEADER      =*-MacroData+1 : !scr "leade",'r'+$80
    M_WANNABE     =*-MacroData+1 : !scr "wannab",'e'+$80
    M_FOAM        =*-MacroData+1 : !scr "foa",'m'+$80
    M_FOREVER     =*-MacroData+1 : !scr "foreve",'r'+$80
    M_BLOCK       =*-MacroData+1 : !scr "bloc",'k'+$80
    M_PICK        =*-MacroData+1 : !scr "pic",'k'+$80
    M_ALL         =*-MacroData+1 : !scr "al",'l'+$80
    M_GAIN11      =*-MacroData+1 : !scr "gain ",78,'1',83,'1'+$80
    M_TWAINPAIN   =*-MacroData+1 : !scr "twain pain game",'s'+$80
!if *-MacroData >= $FF { !error "Out of MacroData memory" }

; Graphic blocks, each ends with 0
BlockData:
    B_FRAMETOP             =*-BlockData : !byte 79,119,119,119,80,0
    B_FRAMEBOTTOM          =*-BlockData : !byte 76,111,111,111,122,0
    B_SPACEHEART           =*-BlockData : !byte 160,211,0
    B_CARDBACKTOP          =*-BlockData : !byte 236,192,192,192,251,0
    B_SHADEDCARDBACKMIDDLE =*-BlockData : !scr ':'
    B_CARDBACKMIDDLE       =*-BlockData : !byte 194,102,102,102,194,0
    B_SHADEDCARDBACKBOTTOM =*-BlockData : !scr ':'
    B_CARDBACKBOTTOM       =*-BlockData : !byte 252,192,192,192,254,0
!if *-BlockData >= $FF { !error "Out of BlockData memory" }

SIZEOF_TEXT=*-TextData

;----------------------------------------------------------------------------
; MAX 2K ALLOWED HERE
;----------------------------------------------------------------------------
!byte 0 ; DUMMY to show where we are in report
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
SuitTextData:
    !byte M_GOBLIN,M_POLYSTYRENE,M_CANDY,M_SOAP
SuitLeaders:
    !byte C_GOBLIN_LEADER,C_POLY_LEADER,C_CANDY_LEADER,C_SOAP_LEADER

; initial PlayerData structure (rest is filled with $FF)
InitData:
    !scr 10, "0/0", 0
SIZEOF_INITDATA = *-InitData

    !fill $30-(*-SuitTextData),0

;----------------------------------------------------------------------------
; INIT CODE (0400-07FF)
;----------------------------------------------------------------------------
!pseudopc $0400 {
            ; TODO add PETSCII logo here in the upper rows? (or just draw text?)
            ; INIT CODE
REALINIT:
!if INIT != REALINIT { !error "REALINIT=", REALINIT, " so update INIT and make file!" }
            cld ; who knows?

            ; setup VIC
            lda #%10011011              ; screen on
            sta $D011
            lda #0                      ; no sprites
            sta $D015
            lda #%00001000              ; hires
            sta $D016
            lda #20                     ; uppercase
            sta $D018
            lda #COL_BORDER
            sta $D020
            lda #COL_SCREEN
            sta $D021
            ; lock uppercase
            lda #$80
            sta $0291

            ; move stack down to gain extra room from $120
            ldx #$1f
            txs

            ; set color of counters
            ldx #2
-           lda #COL_HEALTH_ON
            sta $D800,x
            sta $D800+24*40,x
            lda #COL_PLAIN
            sta $D800+40,x
            sta $D800+23*40,x
            dex
            bpl -

            jmp Start
}
!fill $0400+10*40-(*&$0FFF),0
; put 200 bytes data here
!fill 40*5,34
!fill $0800-(*&$0FFF),0
