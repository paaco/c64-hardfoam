; HARD FOAM - a 2K card game
; Developed for the https://itch.io/jam/the-c64-cassette-50-charity-competition

; Only WRITES memory < $1000 and uses Dxxx IO, calling/reading KERNAL/BASIC is OK

; Note that it is only required to load below $1000, not specifically $0801,
; so we could even load at $0400 (not lower to keep Tape loading compatibility)
; However, loading it there (anything below $0801) will kill RUN, only allow direct SYS
; Exomizer also uses $0334-$03D0 as decrunching buffer; decrunching there will hang

; Without packer it's possible to load and run $120-$1000 giving 3808 bytes instead of 2047 packed
; Holes at $1ED-$01FA, $0314-$032A and $0400-$07E8 (screen)
; Keeping 5 screen rows for code adds 200 bytes

; Usable RAM:  $0120-$0276 (343 bytes) (with stack reduced to $20) / $0293-$02FF (109 bytes)
; Note         $01ED-$200 (19 bytes) are squashed during loading

INTRO=0
DEBUG=1
!ifndef DEBUG {DEBUG=0}
!ifndef INTRO {INTRO=0}
!if DEBUG=1 {
    !initmem $AA
}

;!source "constants.inc" ; older acme doesn't support same-dir includes
BLACK=0
WHITE=1
GREEN=5
BLUE=6
YELLOW=7
ORANGE=8
BROWN=9
LIGHT_RED=10
GREY=12
; colors
COL_BORDER=BROWN
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
!addr TmpPlayerSelectData = PlayerData+PD_DECK+10 ; 6 bytes
!addr Joystick=$90
!addr Index=$91     ; loop/selection index
!addr MaxIndex=$92  ; <MaxIndex
!addr CardYIndex=$93; Y offset used for DrawCardSelect/ClearCardSelect

;############################################################################
*=$0120     ; DATA (0120-01ED = 282 bytes)

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
    !byte 0 ; card# (offsets) should not be 0
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
; PRNG
;----------------------------------------------------------------------------

; Random routine from https://codebase64.org/ 16bit eor shift random generator
; You can get 8-bit numbers in A or 16-bit numbers from the zero page addresses.
; Leaves X/Y unchanged. Init ZP_RNG_LOW<>0
Random:
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
; CHARACTER DRAWING
;----------------------------------------------------------------------------

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
!if (>PutChar != >PutCharNoColor) { !error "PutChar and PutCharNoColor must be in same page" }

; puts cursor at Y/A Y=low byte, A=high byte and sets Y=0 (clobbers A,Y)
SetCursorY0:
            sty _CursorPos
            sty _ColorPos
            sta _CursorPos+1
            eor #$DC                    ; turn 4/5/6/7 into $D8/9/A/B
            sta _ColorPos+1
            ldy #0
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

            !fill 2,$EE ; remaining

;############################################################################
*=$01ED     ; DATA 13 bytes including return address (TRASHED WHILE LOADING)
Overwrite01ED:

; initial PlayerData structure (rest is filled with $FF)
InitData:
    !scr 10, "0/0", 0
SIZEOF_INITDATA = *-InitData

SuitTextData:
    !byte M_GOBLIN,M_POLYSTYRENE,M_CANDY,M_SOAP
SuitLeaders:
    !byte C_POLY_LEADER,C_CANDY_LEADER;,C_SOAP_LEADER,C_GOBLIN_LEADER

*=$01F8     ; Override return value on stack with own start address
            !word INIT-1

;############################################################################
*=$01FA     ; DATA (01FA-0314 = 282 bytes)

;----------------------------------------------------------------------------
; BASIC DRAWING
;----------------------------------------------------------------------------

; puts cursor at Y/A Y=low byte, A=high byte and sets Y=0 and draws text in X (clobbers A,X,Y,Tmp1)
SetCursorDrawTextX:
            jsr SetCursorY0
            txa
; draws text A at cursor + Y+1 (clobbers A,X,Y,Tmp1)
DrawText:
            tax
; draws text X at cursor + Y+1 (clobbers A,X,Y,Tmp1)
DrawTextX:
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

.drawskip:
            iny
            inx
; draws block X at cursor + Y+1 (clobbers A,X,Y)
DrawBlock:
-           lda BlockData,x
            beq +
            cmp #$60                    ; $60/96 (SHIFT-SPACE) skips
            beq .drawskip
            jsr MovePutChar
            inx
            bne -
+           rts

; draws graphic X at cursor + Y+1 and adds 40-(width of card frame) to Y (clobbers A,X,Y,Tmp1)
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

; (0-5) table for position card selection symbols around card
CardSelectionYOffsets:
            !byte 0*6 + 2*40-1
            !byte 1*6 + 2*40-1
            !byte 2*6 + 2*40-1
            !byte 3*6 + 2*40-1
            !byte 4*6 + 2*40-1
            !byte 5*6 + 2*40-1

; set X and Index to Index-1 loop around (Index=0..MaxIndex-1) (clobbers X)
DecIndex:
            ldx Index
            bne +
            ldx MaxIndex
+           dex
            stx Index
            rts

;############################################################################
*=$028D     ; 028D-028E (2 bytes) TRASHED DURING LOADING
            !byte 0,0

; set X and Index to Index+1 loop around (Index=0..MaxIndex-1) (clobbers X)
IncIndex:
            ldx Index
            inx
            cpx MaxIndex
            bne .setidx
            ; fall through

; set MaxIndex to X and set X and Index to 0 (clobbers X)
SetMaxIndexX0:
            stx MaxIndex
            ldx #0
.setidx:    stx Index
            rts

            !fill 4,$EE ; remaining

;############################################################################
*=$02A1     ; RS232 Enables SHOULD STAY 0 DURING LOADING!
            !byte 0

;----------------------------------------------------------------------------
; KEYBOARD / JOYSTICK INPUT
;----------------------------------------------------------------------------

DebounceJoystick:
-           jsr ReadJoystick
            bne -
            rts

; Reads Joystick A/B value (0 active) in A and Joystick variable (clobbers A,X,Y)
;  Z=1/X=0 means no (joystick) key pressed
; If joystick is not active, scans keyboard
ReadJoystick:
            ; disconnect keyboard
            lda #%11111111
            sta $DC00
            ; scan joystick
            lda $DC00           ; Joystick A in control port 2 0=active: 1=up 2=down 4=left 8=right 16=fire
            and $DC01           ; Joystick B in control port 1 0=active: 1=up 2=down 4=left 8=right 16=fire
            ora #%11100000      ; ignore other bits ==> $FF is nothing pressed
            sta Joystick
            tax
            inx                 ; FF+1=0, so Z=1 means no input read
            bne .stealrts2      ; done
            ; fall through

; Reads keyboard and emulates joystick with Cursor, (right) Shift and Return keys (clobbers A,X,Y)
ReadKeyboard:
            ; scan keyboard
            lda #%10111110      ; rows 0 and 6: 7=C_U/D 4=S_R 2=C_L/R 1=CR
            sta $DC00
            ; (Not implemented) row 1 >>2 |Bit 1| S_L |  E  |  S  |  Z  |  4  |  A  |  W  |  3  |
            ; (Not implemented) row 7 >>1 |Bit 7| R/S |  Q  |  C= |SPACE|  2  | CTRL|A_LFT|  1  |
            lda $DC01
            ora #%01101001      ; ignore other bits ==> $FF is nothing pressed
            eor #%11111111      ; 1-active is easier to test double bits
            tay                 ; backup
            ; Fire
            ldx #%11111111
            and #%00001010      ; CR or SPACE?
            beq +               ; no
            ldx #%11101111      ; FIRE
+           stx Joystick
            ; Up/Down
            tya
            and #%10000000      ; C_U/D?
            beq ++              ; no
            ldx #%11111101      ; DOWN
            tya
            and #%00011000      ; SHIFT?
            beq +               ; no
            inx                 ; UP (%11111110)
+           txa
            and Joystick
            sta Joystick
++          ; Left/Right
            tya
            and #%00000100      ; C_L/R
            beq ++
            ldx #%11110111      ; RIGHT
            tya
            and #%00011000      ; SHIFT?
            beq +               ; no
            ldx #%11111011      ; LEFT
+           txa
            and Joystick
            sta Joystick
++          lda Joystick        ; end with joystick in A
            tax
            inx                 ; FF+1=0, so Z=1 means no input read
.stealrts2: rts

            !fill 24,$EE ; remaining

;############################################################################
*=$0314     ; IRQ, BRK and NMI Vectors to keep
            !byte $31,$ea,$66,$fe,$47,$fe
            !byte $4a,$f3,$91,$f2,$0e,$f2
            !byte $50,$f2,$33,$f3,$57,$f1,$ca,$f1
            !byte $ed,$f6 ; STOP vector - Essential to avoid JAM

            ; DATA (032A-0400 = 214 bytes)
            !fill 214,$EE ; remaining

;############################################################################
*=$0400     ; SCREEN (WILL BE WIPED)
INIT:
            ; disable IRQ to avoid KERNAL messing with keyboard
            ldy #%01111111
            sty $dc0d   ; Turn off CIAs Timer interrupts
            sty $dd0d   ; Turn off CIAs Timer interrupts
            lda $dc0d   ; cancel all CIA-IRQs in queue/unprocessed
            lda $dd0d   ; cancel all CIA-IRQs in queue/unprocessed

            ; move stack down to gain extra room from $120
            ldx #$1f
            txs

            ; restore 01ED-01FA
            ldx #SIZEOF_Overwrite01EDCopy-1
-           lda Overwrite01EDCopy,x
            sta Overwrite01ED,x
            dex
            bpl -

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

            ; TODO setup SID

!if INTRO=1 {
            jmp Logo
} else {
            jmp LogoDone
}

LogoDone:
            ; set color of counters
            ldx #3-1
-           lda #COL_HEALTH_ON
            sta $D800,x
            sta $D800+24*40,x
            lda #COL_PLAIN
            sta $D800+40,x
            sta $D800+23*40,x
            dex
            bpl -
            jmp Start

.fixtwainpain:
            ldx #15
            lda #7
-           sta $d884,x
            dex
            bpl -
            rts

Overwrite01EDCopy:
;InitData:
    !scr 10, "0/0", 0
;SuitTextData:
    !byte M_GOBLIN,M_POLYSTYRENE,M_CANDY,M_SOAP
;SuitLeaders:
    !byte C_POLY_LEADER,C_CANDY_LEADER,C_SOAP_LEADER,C_GOBLIN_LEADER
SIZEOF_Overwrite01EDCopy=*-Overwrite01EDCopy

            !fill 23,$EE ; remaining WIPED

*=$0400+3*40+(40-16)/2 ; $0484 above logo so it is still alive
            !scr "twain pain games" ; 16 bytes

            !fill 252,$EE ; remaining WIPED

;############################################################################
*=$0590     ; DATA (MIDDLE 5 SCREEN LINES ARE HIDDEN)

            !fill 200,$EE ; remaining

;############################################################################
*=$0658     ; SCREEN (WILL BE WIPED)

            !fill 200,$EE ; remaining WIPED

*=$0658+5*40 ; in lowest 5*40=200 bytes
Logo:
            jsr .drawlogo
            ; set ptrs to lower part ($0659)
            lda #$59
            sta .logoptr
            sta .logoptr+3
            inc .logoptr+1
            inc .logoptr+3+1
            lda #<(logo+20)
            sta .logosrc
            jsr .drawlogo
            dec .logoptr-2 ; remove #81 from screen

            ; fill everything but the logo blue
--          ldx #0
            lda #BLUE-DEBUG
            .screenptr=*+1
-           ldy $0400,x
---         cmp $d012 ; slow down
            bne ---
            iny
            cpy #82
            beq +
            .colorptr=*+1
            sta $d800,x
+           inx
            cpx #200
            bne -
            lda .screenptr
            clc
            adc #200
            sta .screenptr
            sta .colorptr
            bcc +
            inc .screenptr+1
            inc .colorptr+1
+           cmp #200
            bne +
            jsr .fixtwainpain
+           cmp #$E8 ; end of screen
            bne --

            jsr DebounceJoystick
-           jsr ReadJoystick
            beq -
            jmp LogoDone

.drawlogo:
            ldx #0
--          ldy #10 ; 10 pixels = 8 from the byte and 2 empty
            .logosrc = *+1
-           asl logo,x
            bcc +
            lda #81
            .logoptr = *+1
            sta $0400+1+5*40
            sta $d800+1+5*40
---         cmp $d012 ; slow down
            bne ---
+           inc .logoptr
            inc .logoptr+3
            bne +
            inc .logoptr+1
            inc .logoptr+3+1
+           dey
            bne -
            inx
            cpx #4*5
            bne --
            rts

            !fill 20,$EE ; remaining WIPED

*=$0400+24*40
logo:       !byte %01100110,%00111110,%01111110,%11111110
            !byte %11100111,%01100111,%11100111,%11100111
            !byte %11111111,%11111111,%11111110,%11100111
            !byte %11100111,%11100111,%11101100,%11100111
            !byte %01100110,%01100110,%01100111,%11111110

            !byte %01111111,%01111110,%00111110,%11101110
            !byte %11100000,%11100111,%01100111,%11111111
            !byte %11111100,%11100111,%11111111,%11011011
            !byte %11100000,%11100111,%11100111,%11000011
            !byte %01100000,%01111110,%01100110,%11000110

;############################################################################
*=$07E8     ; CODE

Start:
            jsr ScreenCreateDeck

            jsr DrawHealthBars
            jsr DrawCounters

            lda #GREY
            sta CharCol
            jsr DrawStackSides
            jsr ClearUpperLines
            jsr ClearLowerLines

            ldy #<($0400+15*40+8)
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

            ldy #<($0400+4*40+8)
            lda #>($0400+4*40+8)
            jsr SetCursorY0
            ldx #PlayerData+PD_TABLE ; zP offset into table (increases per 4)
            jsr DrawTable
            ldy #0
            lda #BLACK
            sta CharCol
            lda #102
            jsr FillCard

            jmp *


;----------------------------------------------------------------------------
; GAME SCREENS
;----------------------------------------------------------------------------

;-------------
; CREATE DECK
;-------------
; draws 4 leaders and their cards and lets the user pick a leader
ScreenCreateDeck:
            jsr InitPlayersData
            jsr ClearAll
            lda #WHITE
            sta CharCol
            ldy #<($0400+0*40+9)
            lda #>($0400+0*40+9)
            ldx #T_PICK_LEADER
            jsr SetCursorDrawTextX

            ; Draw decorated leaders in upper part
            ldy #<($0400+2*40+8)
            lda #>($0400+2*40+8)
            jsr SetCursorY0
            ldx #4
            jsr SetMaxIndexX0
-           lda SuitLeaders,x
            jsr DrawCardAOrCardBack
            jsr IncIndex
            bne -

.redrawleader:
            ; show selected card name and effect
            ldy #<($0400+8*40+8)
            lda #>($0400+8*40+8)
            jsr SetCursorY0
            ; wipe previous text
            lda #CHR_SPACE
-           sta (_CursorPos),y
            iny
            cpy #72
            bne -
            ldy #0
            ldx Index
            lda SuitLeaders,x
            sta CardIdx
            jsr DrawCardText

            ; fill array with cards belonging to leader
            lda CardIdx                 ; start with leader
            pha                         ; backup
            jsr SetupSelectionDeck
            jsr DrawSelectionDeck
            pla
            sta CardIdx                 ; restore

            ldy #<($0400+2*40+8)
            lda #>($0400+2*40+8)
            jsr SetCursorY0

            jsr SelectCard
            lda Joystick
            cmp #%11101111              ; FIRE
            beq ScreenPickCards
            bne .redrawleader

;--------------
; SELECT CARDS
;--------------
ScreenPickCards:
            lda CardIdx
            sta PlayerData+PD_DECK
            inc PlayerData+PD_REMAIN
            jsr ClearUpper

            lda #WHITE
            sta CharCol
            ldy #<($0400+18)
            lda #>($0400+18)
            ldx #T_DECK
            jsr SetCursorDrawTextX

            ; part 1: select 4 of leader's deck
            ldy #<($0400+15*40+17)
            lda #>($0400+15*40+17)
            ldx #T_PICK_4
            jsr SetCursorDrawTextX
            jsr PickCards

            ; part 2: select 3 goblins
            lda #WHITE
            sta CharCol
            ldy #<($0400+15*40+13)
            lda #>($0400+15*40+13)
            ldx #T_PICK_3
            jsr SetCursorDrawTextX
            lda #C_GOBLIN_LEADER
            jsr SetupSelectionDeck
            jsr DrawSelectionDeck
            jsr PickCards

            ; part 3: setup deck
            lda #33
            sta ZP_RNG_LOW
            jsr UnpackAndShuffleDeck

            ; TODO part 4: setup opponent

            jmp *

PickCards:
            ldx #6
            jsr SetMaxIndexX0
.redrawcards:
            jsr DrawPickedDeck
            lda $0400+15*40+17+6        ; remaining cards digit on screen
            cmp #'0'
            beq .stealrts3
            ldy #<($0400+17*40+2)
            lda #>($0400+17*40+2)
            jsr SetCursorY0
            ; TODO draw card text ($FF is no text)
            jsr SelectCard
            lda Joystick
+           cmp #%11101111              ; FIRE
            bne .redrawcards
            ; add card to deck
            ldx Index
            lda TmpPlayerSelectData,x   ; card#
            cmp #$FF
            beq .redrawcards            ; already put
            ldy PlayerData+PD_REMAIN
            sta PlayerData+PD_DECK,y
            inc PlayerData+PD_REMAIN
            lda #$FF                    ; remove
            sta TmpPlayerSelectData,x
            dec $0400+15*40+17+6        ; decrease remaining cards digit on screen
            jsr DrawSelectionDeck
            beq .redrawcards            ; always

; create array with selection deck for leader in A (marks all cards as visible)
SetupSelectionDeck:
            ldx #6-1
-           clc
            adc #SIZEOF_CARD            ; next card#
            sta TmpPlayerSelectData,x
            dex
            bpl -
            rts

; draw selection deck card in lower part; if card=$FF then card back is drawn
DrawSelectionDeck:
            ldy #<($0400+17*40+2)
            lda #>($0400+17*40+2)
            jsr SetCursorY0
            sty Tmp4
-           ldx Tmp4
            lda TmpPlayerSelectData,x
            jsr DrawCardAOrCardBack
            inc Tmp4
            lda Tmp4
            cmp #6
            bne -
.stealrts3: rts

; draw currently picked deck in upper part
DrawPickedDeck:
            ldy #<($0400+2*40-1)
            lda #>($0400+2*40-1)
            jsr SetCursorY0
            sty Tmp4
-           ldx Tmp4
            lda PlayerData+PD_DECK,x
            jsr DrawCardAOrEmpty
            dey                         ; remove space between cards
            inc Tmp4
            lda Tmp4
            cmp #8
            bne -
            rts

; select a card from a visible list; debounces and handles left/right
; - assumes cursor is positioned top-left of first card
SelectCard:
            jsr DrawCardSelectIndex
            jsr DebounceJoystick
-           jsr ReadJoystick            ; 111FRLDU
            beq -
            cmp #%11110111              ; RIGHT
            bne +
            jsr IncIndex
+           cmp #%11111011              ; LEFT
            bne +
            jsr DecIndex
+           jmp ClearCardSelectIndex


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

; expands and shuffles PlayerData deck from 8 bytes to 28 cards
UnpackAndShuffleDeck:
            lda PlayerData+PD_DECK      ; leader
            pha                         ; backup
            ldx #7
            ldy #28-1
-           lda PlayerData+PD_DECK,x
            sta PlayerData+PD_DECK,y
            dey
            tya
            and #%00000011              ; use lower 2 bits
            cmp #(28-1)&%00000011       ; to loop 4 times
            bne -
            dex
            bne -
            ; put leader in random place
.random027: jsr Random ; 0-255
            and #$1f   ; 0-31
            cmp #(28-1)+1
            bcs .random027
            tax
            pla
            sta PlayerData+PD_DECK,x
            ; fall through

; Knuth Fisher-Yates shuffle
;    for i in range(0, n-1) inclusive:
;       j = i + random(0, n-1 - i) inclusive
;       swap L[i], L[j]
ShuffleDeck:
            ldx #0                      ; i
            lda #(28-1)+1               ; deck size
            sta Tmp1                    ; "n-1"
.randomI:   jsr Random                  ; 0-255
            and #$1f                    ; 0-31
            cmp Tmp1
            bcs .randomI                ; 0..Tmp2-1
            stx Tmp2                    ; i
            clc
            adc Tmp2                    ; A = j = i + random(0, n-1-i)
            ; swap L[i], L[j]
            ldy PlayerData+PD_DECK,x    ; Y=L[i]
            tax
            lda PlayerData+PD_DECK,x    ; A=L[j]
            sty PlayerData+PD_DECK,x    ; L[j]=L[i]
            ldx Tmp2
            sta PlayerData+PD_DECK,x    ; L[i]=A
            inx
            dec Tmp1                    ; until n-i==0
            bne .randomI                ; which always swaps the last with itself
            rts


;----------------------------------------------------------------------------
; UI DRAWING
;----------------------------------------------------------------------------

; draws 2 lines left of the card backs to simulate the stack (this stays the same during the game)
DrawStackSides:
            ldy #<($0400+3*40)
            lda #>($0400+3*40)
            jsr SetCursorY0
            lda #112
            jsr .put2
            lda #CHR_SPACE
            jsr PutCharMoveDown
            ldy #<($0400+15*40)
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
            ldy #<($0400+21*40)
            lda #>($0400+21*40)
            jsr SetCursorY0
            jsr .lines
            ldx #0
            jmp .spaces

; clears two lines upper lines - starts the lower with 4 line chars (clobbers A,X,Y)
ClearUpperLines:
            ldy #<($0400+2*40)
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
            ldy #<($0400+39)
            lda #>($0400+39)
            jsr SetCursorY0
            lda #10
            sec
            sbc AIData+PD_LIFE
            tax
            lda #COL_HEALTH_OFF * 16 + COL_HEALTH_ON
            jsr .healthbar
            ; lower player
            ldy #<($0400+15*40+39)
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
            ldx #5*40
            lda #CHR_SPACE
-           sta $0400-1+15*40,x
            sta $0400-1+20*40,x
            dex
            bne -
            ; fall through

; wipes the top of the screen completely (clobbers A,X)
ClearUpper:
            ldx #5*40
            lda #CHR_SPACE
-           sta $0400-1,x
            sta $0400-1+5*40,x
            dex
            bne -
            rts

; draws card selection symbols around a card at cursor + Index*6 (clobbers A,X,Y,Tmp1,CardYIndex)
; - cursor is assumed to be top-left of card deck
DrawCardSelectIndex:
            ldx Index
            lda CardSelectionYOffsets,x
            sta CardYIndex
            tay
            lda #COL_SELECTED
            sta CharCol
            ldx #B_SELECTOR
.drawselector:
            jmp DrawBlock

; clears card selection symbols around a card at cursor + CardYIndex (clobbers A,X,Y,Tmp1)
; - cursor is assumed to be top-left of card deck
ClearCardSelectIndex:
            ldy CardYIndex
            ldx #B_CLEARSELECTOR
            bne .drawselector           ; always


;----------------------------------------------------------------------------
; CARD DRAWING
;----------------------------------------------------------------------------

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

; clears the size of a card at cursor + Y+1 (clobbers A,X,Y,Tmp1,Tmp2)
ClearCard:
            lda #CHR_SPACE
; fills the size of a card with A at cursor + Y+1 (clobbers A,X,Y,Tmp1,Tmp2)
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

; draws card on table in TableIdx at cursor + Y+1 (clobbers A,X,Y,Tmp1,Tmp2,CardIdx)
; - draws status border, no decoration and highlighted attack/defense
DrawTableCard:
            ldx TableIdx
            lda TD_CARD,x
            cmp #$FF
            beq ClearCard
+           sta CardIdx
            tax
            jsr SetFrameCharCol
            lda #<PutChar               ; enable color write
            sta .drawvaluefixup1
            ; use disabled color when tapped
            ldx TableIdx
            lda TD_STATUS,x             ; 0=normal, $1=tapped, DISABLED: >=$80 selected
            beq +
            ;bmi .selected
            lda #COL_DISABLED
            ;bne ++                      ; always
;.selected:  lda #COL_SELECTED
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

; draws decorated card in A or Empty Card at cursor + Y+1 (clobbers A,X,Y,Tmp1,Tmp2,CardIdx)
; - draws legend border, full decoration and default attack/defense
DrawCardAOrEmpty:
            cmp #$FF
            beq ClearCard
            bne .drawcard2
; draws decorated card in A or Card Back at cursor + Y+1 (clobbers A,X,Y,Tmp1,Tmp2,CardIdx)
; - draws legend border, full decoration and default attack/defense
DrawCardAOrCardBack:
            cmp #$FF
            beq DrawCardBack
.drawcard2: sta CardIdx
; draws decorated card in CardIdx at cursor + Y+1 (clobbers A,X,Y,Tmp1,Tmp2)
; - draws legend border, full decoration and default attack/defense
DrawCard:
            ldx CardIdx
            jsr SetFrameCharCol
            jsr DrawCardTopFrameDecorated
.drawcard1: ldx CardIdx
            lda Cards+CARD_ATDF,x               ; card values not actuals
            jsr DrawCardBottomDecoration
            lda #92                             ; move to glyph position
            jsr AddAToY
            ldx CardIdx
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
            lda #GREY
            sta CharCol
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
-           ldx #B_FRAMEMIDDLE
            jsr DrawBlockAddModulo
            dec Tmp2
            bne -
            ldx #B_FRAMEBOTTOM
            jmp DrawBlockAddModulo

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
            jmp MovePutChar

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
            jmp MovePutChar

; sets color to frame color (legend/plain) based on LTSC value of Card in X (leaves LTSC value in A)
SetFrameCharCol:
            lda Cards+CARD_LTSC,x
            pha
            bmi + ; legend
            lda #COL_PLAIN
            bne ++
+           lda #COL_LEGEND
++          sta CharCol
            pla
            rts

; sets color to suit color based on LTSC value of Card in X (clobbers A)
SetSuitCharCol:
            lda Cards+CARD_LTSC,x
            lsr
            lsr
            lsr
            lsr
            and #$03
            sta CharCol
            rts


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

    !fill (28-5)*9,$EE ; remaining for total 28 glyphs

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
    T_PICK_4=*-TextData
    !byte M_PICK,M_4,0
    T_PICK_3=*-TextData
    !byte M_AND,M_PICK,M_3,M_GOBLIN,0
    T_DECK=*-TextData
    !byte M_DECK,0
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
    M_DECK        =*-MacroData+1 : !scr "dec",'k'+$80
    M_AND         =*-MacroData+1 : !scr "an",'d'+$80
    M_ALL         =*-MacroData+1 : !scr "al",'l'+$80
    M_4           =*-MacroData+1 : !scr '4'+$80
    M_3           =*-MacroData+1 : !scr '3'+$80
    M_GAIN11      =*-MacroData+1 : !scr "gain ",78,'1',83,'1'+$80
!if *-MacroData >= $FF { !error "Out of MacroData memory" }

; Graphic blocks, each ends with 0; use 96 to skip over a char
BlockData:
    B_FRAMETOP             =*-BlockData : !byte 79,119,119,119,80,0
    B_FRAMEMIDDLE          =*-BlockData : !byte 116,96,96,96,106,0
    B_FRAMEBOTTOM          =*-BlockData : !byte 76,111,111,111,122,0
    B_SPACEHEART           =*-BlockData : !byte 160,211,0
    B_CARDBACKTOP          =*-BlockData : !byte 236,192,192,192,251,0
    B_SHADEDCARDBACKMIDDLE =*-BlockData : !scr ':'
    B_CARDBACKMIDDLE       =*-BlockData : !byte 194,102,102,102,194,0
    B_SHADEDCARDBACKBOTTOM =*-BlockData : !scr ':'
    B_CARDBACKBOTTOM       =*-BlockData : !byte 252,192,192,192,254,0
    B_SELECTOR             =*-BlockData : !scr '>',96,96,96,96,96,'<',0
    B_CLEARSELECTOR        =*-BlockData : !byte 32,96,96,96,96,96,32,0
!if *-BlockData >= $FF { !error "Out of BlockData memory" }


;----------------------------------------------------------------------------
; MAX 2K ALLOWED HERE
;----------------------------------------------------------------------------
!byte 0 ; DUMMY to show where we are in report
!if * >= $1000 { !error "Out of memory" }
