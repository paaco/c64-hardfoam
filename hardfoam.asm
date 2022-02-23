; HARD FOAM - a 4K compressed card game
; Developed for the https://ausretrogamer.com/2022-reset64-4kb-craptastic-game-competition

; TODO: turn: attack with table cards (pick your own order, attack not required)
; TODO: AI turn: take card; if necessary discard the (first)) most expensive one
; TODO: AI turn: while possible, play random cards from hand
; TODO: AI turn: with each table card, attack random table card
; TODO: AI turn: if there are no opponent table cards left, attack player
; TODO: AI end turn

!ifndef DEBUG {DEBUG=1}
!ifndef INTRO {INTRO=0}
!if DEBUG=1 {
    !initmem $AA
}

;!source "constants.inc" ; older acme doesn't support same-dir includes
BLACK=0
WHITE=1
RED=2
CYAN=3
PURPLE=4
GREEN=5
BLUE=6
YELLOW=7
ORANGE=8
BROWN=9
LIGHT_RED=10
DARK_GREY=11
GREY=12
LIGHT_GREEN=13
LIGHT_BLUE=14
LIGHT_GREY=15
; colors
COL_BORDER=BROWN
COL_SCREEN=BLUE
COL_HEALTH_ON=LIGHT_RED
COL_HEALTH_OFF=BLACK
COL_CARDBACK=ORANGE
COL_PLAIN=LIGHT_GREY
COL_LEGEND=YELLOW
COL_DISABLED=LIGHT_BLUE
COL_SELECTED=WHITE
COL_HIGHER=GREEN
COL_LOWER=LIGHT_RED
; characters
CHR_SPACE=32+DEBUG*10 ; space or star
CHR_PLAY=30 ; arrow up
CHR_NO_PLAY=86 ; cross
CHR_ENDTURN=62 ; >
;
HAND_CARDWIDTH=4
TABLE_CARDWIDTH=5
MAX_EFFECT_QUEUE=20

!addr SCREEN=$0400

; ZP addresses
!addr _CursorPos=$02 ; ptr
!addr _ColorPos=$04 ; ptr
!addr CharCol=$06
!addr Suit=$07                          ; Suit and SuitCol are the same
!addr SuitCol=$07
!addr ZP_RNG_LOW = $08
!addr ZP_RNG_HIGH = $09
!addr Tmp1=$0A
!addr Tmp2=$0B
!addr EfTmp=$0B ; reuse Tmp2
!addr Tmp3=$0C
!addr TmpText=$0D
;!addr Suit=$0E
!addr Joystick=$0F
; Player Data (consecutive) (SIZEOF_PD=64 bytes)
!addr PlayerData=$10
    PD_LIFE=0       ; 0 .. 10
    PD_ENERGY=1     ; $30 .. $39
    ; fixed '/' character in between
    PD_MAXENERGY=3  ; $30 .. $39
    PD_REMAIN=4     ; DECKSIZE-1 .. 0
    PD_HAND=5       ; 7 bytes card# or $FF=no card (left filled)
    MAX_HAND=6      ; max #cards in hand
    PD_TABLE=12     ; 20 bytes 5*4 bytes (card#,atk,def,status)
      TD_CARD=0     ; card# $FF=no card
      TD_ATK=1      ; attack
      TD_DEF=2      ; defense
      TD_STATUS=3   ; status bits: 1=tapped, 2=shielded
      STATUS_TAPPED=1
      STATUS_SHIELDED=2
      SIZEOF_TD=4
    MAX_TABLE=5     ; max #cards on table
    PD_DECK=32      ; 32 bytes (card#)
SIZEOF_PD=64
!addr Index=$50     ; loop/selection index
!addr MaxIndex=$51  ; <MaxIndex
!addr SelectorIndex=$52 ; Index used for DrawCardSelect/ClearCardSelect
!addr SelectorIndexBackup=$53
!addr TableIdx=$54  ; loop index for DrawTable
!addr InsertingPos=$55 ; place to insert card during DrawTable ($FF=disable)
!addr InsertingCard=$56 ; card to show during DrawTable
!addr EfQPtr=$57 ; next place in Effect Queue
; Effect interface
!addr EfSource=$58   ; Source of effect (Ptr to card on table or 0 in case of spell)
!addr EfParam=$59    ; Parameter for effect (table or card to apply effect to)
; AI Data (consecutive)
!addr AIData=$90    ; PlayerData+$80 (SIZEOF_PD=64 bytes)
; Draws rectangle 5x5 (upto 8x6) via DrawF function (clobbers A,Y)
!addr _Draw=$E0     ; $E0-$F6 is block drawing routine

; macro to wait for specific raster line to avoid flickering (clobbers A)
!macro WaitVBL .LINE {
            lda #.LINE
.loop       cmp $D012
            !if DEBUG=0 {
                bne .loop
            } else {
                nop
                nop
            }
}

;############################################################################

*=$0801
!byte $0c,$08,<1974,>1974,$9e,$32,$30,$36,$31,$00,$00,$00

start:
            jmp INIT


;----------------------------------------------------------------------------
; CHARACTER DRAWING FUNCTIONS FOR DRAW
;----------------------------------------------------------------------------

; draws card frame in CharCol at Cursor + Y, skipping $60 (clobbers A,X)
DrawF_Frame:
            lda FrameData,x
            inx
            cmp #$60                    ; $60/96 (SHIFT-SPACE) skips
            beq +
DrawF_ACharCol: ; (clobbers A)
            sta (_CursorPos),y
            lda CharCol
            sta (_ColorPos),y
+           rts

; draws glyph in SuitCol at Cursor + Y + 1 (clobbers A,X)
DrawF_Glyph:
            iny
            lda GlyphData,x
            inx
            sta (_CursorPos),y
            lda SuitCol
            sta (_ColorPos),y
            dey
            rts

; draws space in SuitCol at Cursor + Y (clobbers A)
DrawF_ClearSuitCol:
            lda #CHR_SPACE
DrawF_ASuitCol: ; (clobbers A)
            sta (_CursorPos),y
            lda SuitCol
            sta (_ColorPos),y
            rts

!if (>DrawF_Frame != >DrawF_Frame) { !error "All DrawF functions must be in same page" }


;----------------------------------------------------------------------------
; CHARACTER DRAWING
;----------------------------------------------------------------------------

; configures Draw function with X=config (clobbers none)
_Configure:
            pha
            lda ConfigData,x
            sta <.drawfptr
            lda ConfigData+1,x
            sta <.drawwidth
            lda ConfigData+2,x
            sta <.drawheight
            pla
            rts

ConfigData:
    CFG_FRAME=*-ConfigData              ; 5x6 card
    !byte <DrawF_Frame,5,6*40
    CFG_CLEARFRAME=*-ConfigData         ; 5x6 clear
    !byte <DrawF_ClearSuitCol,5,6*40
    CFG_GLYPH=*-ConfigData              ; 3x3 glyph drawn at offset +41
    !byte <DrawF_Glyph,3,4*40
    CFG_FRAME2=*-ConfigData             ; 5x2 card
    !byte <DrawF_Frame,5,2*40
    CFG_GLYPH2=*-ConfigData             ; 3x1 glyph drawn at offset +41 (for 5x2 card)
    !byte <DrawF_Glyph,3,2*40

; configures Draw function with X=config and Draws with A=srcoffset, Y=0(dstoffset) (clobbers A,X,Y)
Draw:
            ldy #0
; configures Draw function with X=config and Draws with A=srcoffset, Y=dstoffset (clobbers A,X,Y)
DrawWithOffset:
            jsr _Configure
            tax
            jmp _Draw

; adds A to cursor (clobbers A,Y)
AddToCursor:
            clc
            adc _CursorPos
            tay
            lda #0
            adc _CursorPos+1
; puts cursor at Y/A Y=low byte, A=high byte (clobbers A)
SetCursor:
            sty _CursorPos
            sty _ColorPos
            sta _CursorPos+1
            eor #$DC                    ; turn 4/5/6/7 into $D8/9/A/B
            sta _ColorPos+1
            rts


;----------------------------------------------------------------------------
; TEXT DRAWING
;----------------------------------------------------------------------------

; ; puts cursor at Y/A Y=low byte, A=high byte and sets Y=0 and draws text in X (clobbers A,X,Y,TmpText)
SetCursorDrawTextX:
            jsr SetCursor
            ldy #0
            txa
; draws text A in SuitCol at Cursor + Y (clobbers A,X,Y,TmpText)
DrawText:
            tax
; draws text X in SuitCol at Cursor + Y (clobbers A,X,Y,TmpText)
DrawTextX:
--          lda TextData,x
            beq ++                      ; text ends with 0
            ; draw macro in A
            stx TmpText
            cmp #M_SUIT                 ; M_SUIT is replaced with text version
            bne +
            ldx Suit
            lda SuitTextData,x
+           tax
-           lda MacroData-1,x
            bpl +
            and #$7F                    ; last character
            ldx #$FF                    ; ends loop
+           jsr DrawF_ASuitCol
            iny
            inx
            bne -
            ldx TmpText
            lda TextData+1,x            ; lookahead
            beq ++                      ; text ends directly with 0
            jsr DrawF_ClearSuitCol      ; put space
            iny
            inx
            bne --
++          rts

SuitTextData:
    !byte M_GOBLIN,M_POLYSTYRENE,M_CANDY,M_SOAP

; clears the two lower lines (clobbers A,Y)
ClearLowerLines: ; 14 bytes
            lda #CHR_SPACE
            ldy #38
-           sta SCREEN+21*40,y
            sta SCREEN+22*40,y
            dey
            bpl -
            rts

; clears two lines upper lines (clobbers A,Y)
ClearUpperLines: ; 14 bytes
            lda #CHR_SPACE
            ldy #38
-           sta SCREEN+2*40,y
            sta SCREEN+3*40,y
            dey
            bpl -
            rts

; wipes the top and bottom of the screen completely (clobbers A,Y)
ClearAll: ; 20 bytes
            lda #CHR_SPACE
            ldy #5*40
-           sta SCREEN-1,y
            sta SCREEN-1+5*40,y
            sta SCREEN-1+15*40,y
            sta SCREEN-1+20*40,y
            dey
            bne -
            rts

; draws energy, deck counters and health bars (clobbers A,X,Y,Tmp1)
DrawCounters:
            ; energy
            ldx #2
-           lda AIData+PD_ENERGY,x
            sta SCREEN,x
            lda PlayerData+PD_ENERGY,x
            sta SCREEN+24*40,x
            dex
            bpl -
            ; deck counters
            lda AIData+PD_REMAIN
            jsr AtoASCII2
            stx SCREEN+1*40
            sta SCREEN+1*40+1
            lda PlayerData+PD_REMAIN
            jsr AtoASCII2
            stx SCREEN+23*40
            sta SCREEN+23*40+1

            ; draw both health bars
            lda #COL_HEALTH_ON
            sta CharCol
            lda #10
            sec
            sbc AIData+PD_LIFE
            tax
            ldy #<(SCREEN+39)
            lda #>(SCREEN+39)
            jsr .healthbar
            lda #COL_HEALTH_OFF
            sta CharCol
            ldx PlayerData+PD_LIFE
            ldy #<(SCREEN+15*40+39)
            lda #>(SCREEN+15*40+39)
            ; fall through
.healthbar:
            stx Tmp1
            jsr SetCursor
            ldx #10
-           cpx Tmp1
            bne +
            ; switch color
            lda CharCol
            eor #COL_HEALTH_ON-COL_HEALTH_OFF ; NOTE: largest-smallest
            sta CharCol
+           lda #$53                    ; heart
            ldy #0
            jsr DrawF_ACharCol
            lda #40
            jsr AddToCursor
            dex
            bne -
            rts

; Converts .A to 3 ASCII/PETSCII digits: .Y = hundreds, .X = tens, .A = ones
AtoASCII2: ; 20 bytes (for 2 digits)
            ; ldy #$2f
            ldx #$3a
            sec
; -           iny
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
            ; scan joysticks
            lda $DC00           ; Joystick A in control port 2 0=active: 1=up 2=down 4=left 8=right 16=fire
            and $DC01           ; Joystick B in control port 1 0=active: 1=up 2=down 4=left 8=right 16=fire
            ora #%11100000      ; ignore other bits ==> $FF is nothing pressed
            sta Joystick
            tax
            inx                 ; FF+1=0, so Z=1 means no input read
            bne .stealrts2      ; done
            ; fall through

; Reads keyboard and emulates joystick with Cursor, (right) Shift and Return keys (clobbers A,X,Y)
ReadKeyboardAsJoystick:
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
            and #%00000100      ; C_L/R?
            beq ++              ; no
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
; INIT
;----------------------------------------------------------------------------

INIT:
            ; disable IRQ to avoid KERNAL messing with keyboard
            ldy #%01111111
            sty $dc0d   ; Turn off CIAs Timer interrupts
            sty $dd0d   ; Turn off CIAs Timer interrupts
            lda $dc0d   ; cancel all CIA-IRQs in queue/unprocessed
            lda $dd0d   ; cancel all CIA-IRQs in queue/unprocessed

            ; copy Draw function to ZP
            ldx #SIZEOF_DRAW-1
-           lda INITDRAW,x
            sta _Draw,x
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

; Draws rectangle 5x5 (upto 8x6) via DrawF function, offset in Y (clobbers A,Y)
INITDRAW:
!pseudopc _Draw {
            ;ldy #0 ; never used
            .drawfptr=*+1
-           jsr DrawF_Frame             ; (1) change low byte of ptr to other routine
            iny
            tya
            and #%00000111
            .drawwidth=*+1
            cmp #5                      ; (2) change width
            bne -
            tya
            and #%11111000
            adc #40-1                   ; C=1 always because of cmp
            tay
            .drawheight=*+1
            cmp #6*40                   ; (3) change height to 2*40 to plot 5x2 rows
            bne -
            rts
}
SIZEOF_DRAW=*-INITDRAW


;----------------------------------------------------------------------------
; GAME FUNCTIONS
;----------------------------------------------------------------------------

; reset all player data (energy, deck, table)
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

; initial PlayerData structure (rest is filled with $FF)
InitData:
    !scr 10, "0/0", 28
SIZEOF_INITDATA = *-InitData

; copy deck# in A into PlayerData deck, unpacks and shuffles (clobbers A,X,Y)
CreatePlayerDeck:
            asl
            asl
            asl                         ; 0,8,16,24
            tax
            ldy #0
-           lda Decks,x
            sta PlayerData+PD_DECK,y
            inx
            iny
            cpy #8
            bne -
            ; fall through

; expands and shuffles PlayerData deck from 8 bytes to 28 cards (clobbers A,X,Y)
UnpackAndShufflePlayerDeck:
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
.random027: jsr Random                  ; 0-255
            ;and #$1f                    ; 0-31
            cmp #(28-1)+1
            bcs .random027
            tax
            pla
            sta PlayerData+PD_DECK,x
            ; fall through

; Knuth Fisher-Yates shuffle Player deck (clobbers A,X,Y,Tmp1,Tmp2)
;    for i in range(0, n-1) inclusive:
;       j = i + random(0, n-1 - i) inclusive
;       swap L[i], L[j]
ShufflePlayerDeck:
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

; set X and Index to Index-1 loop around (Index=0..MaxIndex-1) (clobbers X)
DecIndex:
            ldx Index
            bne +
            ldx MaxIndex
+           dex
            stx Index
            rts

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

; select a card from a visible list; debounces and handles left/right
; - assumes cursor is positioned top-left of first card
SelectCard:
            jsr DrawSelector
            jsr DebounceJoystick
-           jsr ReadJoystick            ; 111FRLDU
            beq -
            cmp #%11110111              ; RIGHT
            bne +
            jsr IncIndex
+           cmp #%11111011              ; LEFT
            bne +
            jsr DecIndex
+           ; fall through

; clears card selection symbols around a card at Cursor + SelectorIndex*6 (clobbers A,X,Y)
; - cursor is assumed to be top-left of card deck
ClearSelector:
            ldx SelectorIndex
            jsr .drawselectorspace
            inx
.drawselectorspace:
            lda #CHR_SPACE
            bne .drawselector           ; always

; draws selection symbols around a card at Cursor + Index*6 (clobbers A,X,Y)
; - cursor is assumed to be top-left of card deck
DrawSelector:
            lda #COL_SELECTED
            sta CharCol
            ldx Index
            stx SelectorIndex
            lda #'>'
            jsr .drawselector
            inx
            lda #'<'
.drawselector:
            ldy CardSelectorOffsets,x
            jmp DrawF_ACharCol

CardSelectorOffsets: ; max 6 cards can be selected (index 5)
    !for i,0,5 { !byte 2*40-1 + i*6 }


;----------------------------------------------------------------------------
; START
;----------------------------------------------------------------------------

Start:
            jsr ClearAll
            jsr InitPlayersData

            ; DEBUG
            lda #33                     ; TODO seed via D418
            sta ZP_RNG_LOW              ; seed prng with some value

            ; setup random AI name
            jsr Random
            and #$03                    ; 0..3 (name#)
            asl
            tax
            lda AINames,x
            sta opponent_name1
            lda AINames+1,x
            sta opponent_name2

            ; draw "your opponent is"
            lda #COL_PLAIN
            sta SuitCol
            ldy #<(SCREEN+5*40+9)
            lda #>(SCREEN+5*40+9)
            ldx #T_YOUR_OPPONENT_IS ; 21
            jsr SetCursorDrawTextX

            ; pick random AI deck
            jsr Random
            and #$03                    ; 0..3 (deck#)
            jsr CreatePlayerDeck
            ; copy to AI deck
            ldx #28-1
-           lda PlayerData+PD_DECK,x
            sta AIData+PD_DECK,x
            dex
            bpl -

            ; pick your deck
            ldy #<(SCREEN+9*40+(40-17)/2)
            lda #>(SCREEN+9*40+(40-17)/2)
            ldx #T_PICK_DECK ; 17
            jsr SetCursorDrawTextX
            ; invert at cursor
            ldy #17
-           dey
            lda (_CursorPos),y
            eor #$80
            sta (_CursorPos),y
            cpy #$00
            bne -

            ; draw first card of each deck (8 bytes apart)
            ldy #<(SCREEN+15*40+(40-24)/2)
            lda #>(SCREEN+15*40+(40-24)/2)
            jsr SetCursor
            ldy #0
-           sty Tmp1
            ldx Decks,y
            jsr DrawCard
            lda #6
            jsr AddToCursor
            lda Tmp1
            clc
            adc #8
            tay
            cpy #4*8
            bne -

            ; select deck
            ldx #4
            jsr SetMaxIndexX0
-           lda Index
            sta Suit
            jsr ClearLowerLines
            ldy #<(SCREEN+21*40+(40-24)/2)
            lda #>(SCREEN+21*40+(40-24)/2)
            ldx #T_SUIT_DECK
            jsr SetCursorDrawTextX

            ldy #<(SCREEN+15*40+(40-24)/2)
            lda #>(SCREEN+15*40+(40-24)/2)
            jsr SetCursor
!if DEBUG=0 {
            jsr SelectCard
            lda Joystick
            cmp #%11101111              ; FIRE
            bne -
}
            ; create selected deck as player deck
            lda Index                   ; 0..3
            jsr CreatePlayerDeck

            ; pull first 3 cards for both
            ldy #3
-           jsr PullPlayerDeckCard
            jsr PullAIDeckCard
            dey
            bne -

            ; redraw screen with opponent at top
            jsr ClearAll
            lda #COL_PLAIN
            sta SuitCol
            ldx #T_OPPONENT_NAME
            ldy #<(SCREEN+0*40+4)
            lda #>(SCREEN+0*40+4)
            jsr SetCursorDrawTextX
            ldy #<(SCREEN+4*40)
            lda #>(SCREEN+4*40)
            jsr SetCursorDrawCardBack
            jsr DrawAIHand

NextRound:
            ; pull next card
            jsr PullPlayerDeckCard

            ; restore energy for player
            ldx #PlayerData
            jsr Energize
            jsr DrawCounters

            ; TODO: start of turn: select first possible hand card
            ; TODO: if there is none, select first table card
            ; TODO: if there is none, select END symbol

            ; select from hand
            lda #0                      ; 0..MAX_HAND-1=card, MAX_HAND=END
            sta SelectorIndex
            sta SelectorIndexBackup

            ; draw selected card for player (or card back)
.redraw:    ldy #<(SCREEN+15*40)
            lda #>(SCREEN+15*40)
            jsr SetCursor
            +WaitVBL($30)
            jsr ClearLowerLines         ; clears text area
            ldy SelectorIndex
            cpy #MAX_HAND+1
            bcc +
            ldy SelectorIndexBackup
            sty SelectorIndex
+           ldx PlayerData+PD_HAND,y
            sty SelectorIndexBackup
            cpx #$FF
            bne +
            jsr DrawCardBack
            jmp ++
+           txa
            pha
            jsr DrawCard
            pla
            tax
            ldy #<(SCREEN+21*40)
            lda #>(SCREEN+21*40)
            jsr SetCursorDrawCardText

++          jsr DrawPlayerHand
            lda #COL_SELECTED
            sta SuitCol
            ldy #<(SCREEN+23*40+35)
            lda #>(SCREEN+23*40+35)
            ldx #T_END
            jsr SetCursorDrawTextX

            ; draw selection symbol
            ldy #<(SCREEN+23*40)
            lda #>(SCREEN+23*40)
            jsr SetCursor
            ldx SelectorIndex
            cpx #MAX_HAND
            bne +
            lda #CHR_ENDTURN
            bne ++                      ; always
+           ldy PlayerData+PD_HAND,x
            cpy #$FF                    ; is there a card?
            beq .no_energy              ; no card -> handle as if no energy
            lda Cards+CARD_LTSC,y
            and #%00001111              ; LTSSCCCC
            ora #$30                    ; $30..$39 as energy
            cmp PlayerData+PD_ENERGY
            beq .can_afford
            bcc .can_afford
.no_energy: lda #CHR_NO_PLAY
            bne ++                      ; always
.can_afford:lda #CHR_PLAY
++          ldy HandSelectorOffsets,x
            jsr DrawF_ASuitCol

;!if DEBUG=0 {
            jsr DebounceJoystick
-           jsr ReadJoystick            ; 111FRLDU
            beq -
            cmp #%11110111              ; RIGHT
            bne +
            inc SelectorIndex
+           cmp #%11111011              ; LEFT
            bne +
            dec SelectorIndex
+           cmp #%11101111              ; FIRE
            bne +

            ldx SelectorIndex
            ldy HandSelectorOffsets,x
            ; end of turn?
            lda #CHR_ENDTURN
            cmp (_CursorPos),y
            bne +                       ; nope
            ; end of turn
            ldy #PlayerData+PD_TABLE
            jsr UntapTable
            jsr DrawPlayerTable
            jmp NextRound

            ; play card?
+           lda #CHR_PLAY
            cmp (_CursorPos),y
            bne +                       ; not possible
;}
            ; cast card X from hand
            jsr CastPlayerCard
            ; redraw screen
            jsr DrawPlayerHand
            jsr DrawCounters
            jsr DrawPlayerTable
            ; run all queued effects
-           jsr RunEffect
            jsr DrawCounters
            jsr DrawPlayerTable ; TODO should this be part of RunEffect somehow?
            ldy EfQPtr
            bne -

+           jmp .redraw

HandSelectorOffsets:
    !for i,0,MAX_HAND { !byte 10 + i * HAND_CARDWIDTH }


;----------------------------------------------------------------------------
; GAME FUNCTIONS
;----------------------------------------------------------------------------

; pull a card in hand for Player (clobbers A,X) returns 0 when no cards were left
; if there's no room in hand, the card is lost
PullPlayerDeckCard:
            lda PlayerData+PD_REMAIN
            beq +                       ; no cards left
            dec PlayerData+PD_REMAIN
            ; is hand full?
            lda PlayerData+PD_HAND+MAX_HAND-1
            cmp #$FF
            bne +                       ; hand full (A will be <>0 because card# is never 0)
            ; move cards to make room at start of hand
            ldx #MAX_HAND-2
-           lda PlayerData+PD_HAND,x
            sta PlayerData+PD_HAND+1,x
            dex
            bpl -
            ; store top card of deck at start of hand
            ldx PlayerData+PD_REMAIN
            lda PlayerData+PD_DECK,x
            sta PlayerData+PD_HAND      ; card# is never 0 so A<>0
+           rts

; pull a card in hand for AI (clobbers A,X) returns 0 when no cards were left
; if there's no room in hand, the card is lost
PullAIDeckCard:
            lda AIData+PD_REMAIN
            beq +                       ; no cards left (A will be 0)
            dec AIData+PD_REMAIN
            ; is hand full?
            lda AIData+PD_HAND+MAX_HAND-1
            cmp #$FF
            bne +                       ; hand full (A will be <>0 because card# is never 0)
            ; move cards to make room at start of hand
            ldx #MAX_HAND-2
-           lda AIData+PD_HAND,x
            sta AIData+PD_HAND+1,x
            dex
            bpl -
            ; store top card of deck at start of hand
            ldx AIData+PD_REMAIN
            lda AIData+PD_DECK,x
            sta AIData+PD_HAND          ; card# is never 0 so A<>0
+           rts

; increase max energy and replenish energy of Player in X (clobbers Y)
Energize:
            ldy PD_MAXENERGY,x
            cpy #$39
            beq +
            iny
            sty PD_MAXENERGY,x
+           sty PD_ENERGY,x
            rts

; decrease energy of Player in X by A (clobbers A) returns C=1 success, C=0 failed
DecreaseEnergy:
            clc
            sbc PD_ENERGY,x             ; A=A-(PD_ENERGY-1)
            eor #$FF                    ; $100-A
            cmp #$30
            bcc +
            sta PD_ENERGY,x
+           rts

; cast card #X from hand of Player (clobbers A,X,Y)
CastPlayerCard:
            ldy PlayerData+PD_HAND,x    ; card# in Y
            ; remove card by shifting cards X..MAX_HAND-1 left
-           cpx #MAX_HAND-1             ; 0..MAX_HAND-1
            beq +
            lda PlayerData+PD_HAND+1,x
            sta PlayerData+PD_HAND,x
            inx
            bne -                       ; "always"
+           lda #$FF
            sta PlayerData+PD_HAND,x    ; wipe last card
            ; TODO howto "uncast"?
            ; TODO howto select card target (that even might be cancelled or not exist?)
            lda Cards+CARD_LTSC,y
            and #%00001111              ; LTSSCCCC
            ldx #PlayerData
            jsr DecreaseEnergy
            lda Cards+CARD_LTSC,y
            and #%01000000              ; LTSSCCCC T=Type(0=Spell/1=Monster)
            beq .spell
            ; put monster card on table
            ; TODO select the card index on Table? / can you cancel putting a card down?
            lda #PlayerData+PD_TABLE    ; table
            ldx #0                      ; card index on table
            jsr PutCardOnTable
            ; monster: Y=card# / X=PlayerData+PD_TABLE+card#*SIZEOF_TD("self") / A=1 (STATUS_TAPPED)
            ; spell:   Y=card# / X=PlayerData / A=0
.spell:     lda Cards+CARD_EFFECT,y
            jmp QueueEffect

; put card Y on Table A at index X (0..MAX_TABLE-1) (clobbers A,X,Tmp1)
; Note that this blindly assumes there's room on the table!
PutCardOnTable:
            pha
            clc
            adc TableCardOffsets,x      ; => A is index to card on Table (AI/PlayerData+PD_TABLE+X*SIZEOF_TD)
            sta Tmp1
            pla
            ; make room for card on table
            ;clc
            adc #(MAX_TABLE-1)*SIZEOF_TD-1 ; last byte of 2nd last card
            tax
-           lda TD_CARD,x
            sta TD_CARD+SIZEOF_TD,x
            dex
            cpx Tmp1
            bpl -
            inx                         ; fixup (X is now Tmp1)
            ; place card
            sty TD_CARD,x
            lda Cards+CARD_ATDF,y       ; AAAADDDD : AAAA=Attack DDDD=Defense
            lsr
            lsr
            lsr
            lsr
            sta TD_ATK,x
            lda Cards+CARD_ATDF,y       ; AAAADDDD : AAAA=Attack DDDD=Defense
            and #$0F
            sta TD_DEF,x
            lda #STATUS_TAPPED
            sta TD_STATUS,x             ; status bits: 1=tapped, 2=shielded
            rts

TableCardOffsets:
            !for i,0,MAX_TABLE-1 { !byte i * SIZEOF_TD }

; untap all cards on table in Y (clobbers A,X,Y)
UntapTable:
            ldx #0
-           lda TD_CARD,y
            cmp #$FF
            beq +
            lda TD_STATUS,y
            and #255-STATUS_TAPPED
            sta TD_STATUS,y
+           tya
            clc
            adc #SIZEOF_TD
            tay
            inx
            cpx #MAX_TABLE
            bne -
            rts


;----------------------------------------------------------------------------
; EFFECTS
;----------------------------------------------------------------------------
; Effect interface
; !addr EfSource=$58   ; Source of effect (Ptr to card on table or 0 in case of spell)
; !addr EfParam=$59    ; Parameter for effect (table or card to apply effect to)

; Queue effect A with Source X and Param Y (clobbers A,Y,EfTmp)
QueueEffect:
            sty EfTmp
            ldy EfQPtr
            inc EfQPtr
!if DEBUG=1 {
            cpy #MAX_EFFECT_QUEUE
            bne +
-           inc $D020                   ; Hang if Queue exhausted
            jmp -
+           }
            sta EQueueEffect,y
            txa
            sta EQueueSource,y
            lda EfTmp
            sta EQueueParam,y
.stealrts3: rts

; applies first effect from the Effect Queue (clobbers A,X,Y)
RunEffect:
            ldy EfQPtr
            beq .stealrts3
            dec EfQPtr
            ldy EfQPtr
            lda EQueueSource,y
            sta EfSource
            lda EQueueParam,y
            sta EfParam
            lda EQueueEffect,y

            ; hard coded jump list
            cmp #E_READY
            bne +
; untap Source (clobbers A,X)
.Effect_Untap:
            ldx EfSource
            lda TD_STATUS,x
            and #255-STATUS_TAPPED
            sta TD_STATUS,x
            rts

+           cmp #E_ALL_GAIN11
            bne +
; for each card on table (not EfSource) that matches the Suit of EfSource post effect TODO post effect Param
.Effect_AllGain11:
            ldx EfSource
            ldy TD_CARD,x
            lda Cards+CARD_LTSC,y
            and #%00110000              ; LTSSCCCC SS=Suit(0,1,2,3)
            sta Tmp3                    ; Suit
            txa
            and #$80
            ora #PlayerData+PD_TABLE
            tax                         ; first card on table
            cpx EfSource
            beq ++                      ; skip self
-           ldy TD_CARD,x
            cpy #$FF                    ; empty
            beq ++
            lda Cards+CARD_LTSC,y
            and #%00110000              ; LTSSCCCC SS=Suit(0,1,2,3)
            cmp Tmp3
            bne ++                      ; skip wrong suit
            lda #E_INTERNAL_ALLGAIN11
            jsr QueueEffect             ; Queue effect A with Source X and Param Y (clobbers A,Y,EfTmp)
++          txa
            clc
            adc #SIZEOF_TD
            tax
            and #$7F                    ; remove Player
            cmp #PlayerData+PD_TABLE+MAX_TABLE*SIZEOF_TD
            bne -
            rts

+           cmp #E_INTERNAL_ALLGAIN11
            bne +
; inc ATK and inc DEF of Source (clobbers X) TODO limit values
.Effect_Gain11:
            ldx EfSource
            inc TD_ATK,x
            inc TD_DEF,x
+           rts


;----------------------------------------------------------------------------
; UI DRAWING
;----------------------------------------------------------------------------

; draw upper hand with AI card backs (clobbers A,X,Y,Index)
DrawAIHand:
            lda #AIData+PD_HAND
            sta .fixuphandptr
            lda #<DrawPartialCardBack
            sta .fixupdrawcard
            ldy #<(SCREEN+0*40+8)
            lda #>(SCREEN+0*40+8)
            bne .drawhand               ; always

; draws lower hand with visible Player cards (clobbers A,X,Y,Index)
DrawPlayerHand:
            lda #PlayerData+PD_HAND
            sta .fixuphandptr
            lda #<DrawPartialCard
            sta .fixupdrawcard
            ldy #<(SCREEN+23*40+8)
            lda #>(SCREEN+23*40+8)
.drawhand:
            jsr SetCursor
            ldx #MAX_HAND
            jsr SetMaxIndexX0
            .fixuphandptr=*+1
-           lda AIData+PD_HAND,x        ; SELF-MODIFIED
            cmp #$FF
            bne +
            cpx #0                      ; no cards?
            bne .spacehand
            dec _CursorPos              ; fixup
            bne .spacehand              ; always
+           tax
            .fixupdrawcard=*+1
            jsr DrawPartialCardBack     ; SELF-MODIFIED
            lda #HAND_CARDWIDTH
            jsr AddToCursor
+           jsr IncIndex
            bne -
.spacehand: ; fill remainder after cursor with spaces
-           lda _CursorPos
            cmp #38                     ; end for AI hand
            beq +
            cmp #<(23*40+38)            ; end for Player hand
            beq +
            inc _CursorPos
            lda #CHR_SPACE
            ldy #0
            sta (_CursorPos),y
            ldy #40
            sta (_CursorPos),y
            bne -                       ; always
+           rts

; draw lower table
DrawPlayerTable:
            ldy #<(SCREEN+15*40+(40-24)/2)
            lda #>(SCREEN+15*40+(40-24)/2)
            jsr SetCursor
            ldy #PlayerData+PD_TABLE
; draw table in Y at Cursor (clobbers A,X,Y,TableIdx,Index)
.drawtable:
            ldx #$FF                    ; hide inserting by default
; draw table in Y at Cursor inserting card A at position X (clobbers A,X,Y,TableIdx,Index)
DrawTableInserting:
            stx InsertingPos
            sta InsertingCard
            ldx #MAX_TABLE
            jsr SetMaxIndexX0
-           sty TableIdx                ; $1C or $5C
            cpx InsertingPos
            beq .drawinsert
            jsr DrawTableCard
            lda #TABLE_CARDWIDTH+1
            jsr AddToCursor
            lda TableIdx
            clc
            adc #SIZEOF_TD
            tay
--          jsr IncIndex
            bne -
            rts
.drawinsert:
            ldx InsertingCard
            jsr DrawCard
            lda #TABLE_CARDWIDTH+1
            jsr AddToCursor
            ldy TableIdx
            bne --                      ; always


;----------------------------------------------------------------------------
; CARD DRAWING
;----------------------------------------------------------------------------

; draws card on table in Y at Cursor (clobbers A,X,Y)
; - draws status border, no decoration and highlighted attack/defense
DrawTableCard:
            ldx TD_CARD,y
            cpx #$FF
            bne +
            ldx #CFG_CLEARFRAME         ; clear card
            jmp Draw
+           jsr DecorateBottomFrame     ; set (default) atk/def values
            ; hide type/cost
            lda #79                     ; left corner
            sta frame_TYPE
            lda #119                    ; top line
            sta frame_COST
            ; override colors if tapped
            lda #$85                    ; STA
            sta .fixupcolorwrite        ; enable color write
            lda TD_STATUS,y             ; status bits: 1=tapped, 2=shielded
            and #STATUS_TAPPED
            beq +
            lda #COL_DISABLED
            sta CharCol
            sta SuitCol
            lda #$24                    ; BIT
            sta .fixupcolorwrite        ; disable color write
+           tya                         ; backup card in Y
            pha
            jsr .drawglyphframe
            pla
            tax                         ; restore card to X
            ; update attack value with actual
            ldy #5*40+1                 ; ATK position
            jsr .updateatkdef
            ; update defense value with actual
            inx ; CAREFUL: THIS ASSUMES DEFENSE COMES DIRECTLY AFTER ATTACK IN MEMORY
            ldy #5*40+4                 ; DEF position
            ; fall through
; common code to read screen to determine high/low/same, X=table index
.updateatkdef:
            lda (_CursorPos),y
            and #$0F
            cmp TD_ATK,x
            beq .stealrts1              ; same -> done
            bcc .higher
            lda #COL_LOWER
            bne +                       ; always
.higher:    lda #COL_HIGHER
            .fixupcolorwrite=*
+           sta CharCol                 ; SELF-MODIFIED $85=STA $24=BIT
            lda TD_ATK,x
            ora #$30                    ; regular digits
            jmp DrawF_ACharCol

; decorates top and bottom of frame according to card in X (clobbers A)
DecorateFrame:
            ; setup type/cost
            lda Cards+CARD_LTSC,x
            pha
            and #%01000000              ; LTSSCCCC
            asl
            asl                         ; C=T (0=spell, 1=monster)
            adc #$D7                    ; T=0->D7(ball) T=1->D8(clover)
            sta frame_TYPE
            pla
            and #%00001111              ; LTSSCCCC
            ora #$B0                    ; 0..9 reversed
            sta frame_COST
            ; fall through

; decorates bottom of frame according to card in X (clobbers A)
DecorateBottomFrame:
            ; setup ATK/DEF
            lda Cards+CARD_ATDF,x
            pha
            and #$0F
            ora #$B0
            sta frame_DEF
            pla
            lsr
            lsr
            lsr
            lsr
            ora #$B0
            sta frame_ATK
            ; fall through

; sets CharCol and SuitCol according to card in X (clobbers A)
SetColors:
            ; setup colors
            lda #COL_LEGEND
            sta CharCol
            lda Cards+CARD_LTSC,x
            pha
            bmi + ; legend
            lda #COL_PLAIN
            sta CharCol
+           pla
            lsr
            lsr
            lsr
            lsr
            and #$03
            sta SuitCol
.stealrts1: rts

; draws decorated card in X at Cursor (clobbers A,X,Y)
; - draws legend border, full decoration and default attack/defense
DrawCard:
            jsr DecorateFrame
.drawglyphframe:
            lda Cards+CARD_GLYPH,x
            ldx #CFG_GLYPH
            ldy #40
            jsr DrawWithOffset
            lda #FRAME_CARD
            ldx #CFG_FRAME
            jmp Draw

; puts cursor at Y/A Y=low byte, A=high byte, card background (clobbers A,X,Y)
SetCursorDrawCardBack:
            jsr SetCursor
; draws card background at Cursor (clobbers A,X,Y)
DrawCardBack:
            lda #COL_CARDBACK
            sta CharCol
            lda #FRAME_CARDBACK
            ldx #CFG_FRAME
            jmp Draw

            !align $0F,0,0 ; force DrawPartialCard* in same

; draws top 2-lines of card background at Cursor (clobbers A,X,Y)
DrawPartialCardBack:
            lda #COL_CARDBACK
            sta CharCol
            lda #FRAME2_CARDBACK
            ldx #CFG_FRAME2
            jmp Draw

; draws top 2-lines of decorated card in X at Cursor (clobbers A,X,Y)
; - draws legend border, full decoration and default attack/defense
DrawPartialCard:
            jsr DecorateFrame
            ; override color if cost too high
            lda PlayerData+PD_ENERGY    ; $30..$39 energy
            ora #$B0                    ; $B0..$B9 (same as on card)
            cmp frame_COST              ; energy-cost
            bpl +                       ; 0+ OK
            lda #COL_DISABLED
            sta CharCol
            sta SuitCol
+           lda Cards+CARD_GLYPH,x
            ldx #CFG_GLYPH2
            ldy #40
            jsr DrawWithOffset
            lda #FRAME2_CARD
            ldx #CFG_FRAME2
            jmp Draw
!if (>DrawPartialCardBack != >DrawPartialCard) { !error "DrawPartialCard and DrawPartialCardBack must be in same page" }

; puts cursor at Y/A Y=low byte, A=high byte, draw text of card in X (clobbers A,X,Y,Tmp1)
SetCursorDrawCardText:
            jsr SetCursor
; draws text of card in X at Cursor (clobbers A,X,Y,Tmp1)
DrawCardText:
            txa
            pha                         ; backup X
            jsr SetColors
            ldy #0
            lda Cards+CARD_NAME,x
            jsr DrawText
            pla
            tax                         ; restore X
            ldy #40
            lda Cards+CARD_EFFECT,x
            jmp DrawText


;----------------------------------------------------------------------------
; CARDS
;----------------------------------------------------------------------------
CARD_LTSC=0         ; 1 byte LTSSCCCC : L=Legendary T=Type(0=Spell/1=Monster) SS=Suit(0,1,2,3) CCCC=Cost(0..15)
CARD_ATDF=1         ; 1 byte AAAADDDD : AAAA=Attack DDDD=Defense
CARD_NAME=2         ; 1 byte Name TextPtr
CARD_EFFECT=3       ; 1 byte Effect TextPtr (also used to perform effect)
CARD_GLYPH=4        ; 1 byte GlyphPtr
SIZEOF_CARD=5       ; 256/5 => 50 cards max (0 and $FF are used for other purposes)

; TODO add Sen'jin Shieldmasta ("Taz'dingo") 4 3/5
Cards:
    !byte 0 ; card# (offsets) should not be 0
    C_GOBLIN_LEADER=*-Cards
    !byte $C3, $32, N_GOBLIN_LEADER, E_ALL_GAIN11, G_LEGND_GOBLIN
    !byte $41, $12, N_WANNABE,       E_READY,      G_WANNABE
    !byte $02, $12, N_WANNABE,       T_NONE,       G_3
    !byte $03, $13, N_WANNABE,       T_NONE,       G_4
    !byte $41, $14, N_WANNABE,       T_NONE,       G_5
    !byte $02, $15, N_WANNABE,       T_NONE,       G_6
    !byte $03, $16, N_WANNABE,       T_NONE,       G_7
    !byte $03, $17, N_WANNABE,       T_NONE,       G_8
    C_POLY_LEADER=*-Cards
    !byte $D3, $17, N_POLY_LEADER,   T_NONE,       G_LEGND_POLY
    !byte $51, $12, N_WANNABE,       T_NONE,       G_WANNABE
    !byte $52, $12, N_WANNABE,       T_NONE,       G_WANNABE
    !byte $53, $12, N_WANNABE,       T_NONE,       G_WANNABE
    !byte $54, $12, N_WANNABE,       T_NONE,       G_WANNABE
    !byte $55, $12, N_WANNABE,       T_NONE,       G_WANNABE
    !byte $56, $12, N_WANNABE,       T_NONE,       G_WANNABE
    !byte $57, $12, N_WANNABE,       T_NONE,       G_WANNABE
    C_CANDY_LEADER=*-Cards
    !byte $E0, $00, N_CANDY_LEADER,  T_NONE,       G_LEGND_CANDY
    !byte $61, $12, N_WANNABE,       T_NONE,       G_WANNABE
    !byte $62, $12, N_WANNABE,       T_NONE,       G_WANNABE
    !byte $63, $12, N_WANNABE,       T_NONE,       G_WANNABE
    !byte $64, $12, N_WANNABE,       T_NONE,       G_WANNABE
    !byte $65, $12, N_WANNABE,       T_NONE,       G_WANNABE
    !byte $66, $12, N_WANNABE,       T_NONE,       G_WANNABE
    !byte $67, $12, N_WANNABE,       T_NONE,       G_WANNABE
    C_SOAP_LEADER=*-Cards
    !byte $F0, $00, N_SOAP_LEADER,   T_NONE,       G_LEGND_SOAP
    !byte $71, $12, N_WANNABE,       T_NONE,       G_WANNABE
    !byte $72, $12, N_WANNABE,       T_NONE,       G_WANNABE
    !byte $73, $12, N_WANNABE,       T_NONE,       G_WANNABE
    !byte $74, $12, N_WANNABE,       T_NONE,       G_WANNABE
    !byte $75, $12, N_WANNABE,       T_NONE,       G_WANNABE
    !byte $76, $12, N_WANNABE,       T_NONE,       G_WANNABE
    !byte $77, $12, N_WANNABE,       T_NONE,       G_WANNABE
NUM_CARDS=(*-Cards)/5

; default decks (8 bytes each, starting with legendary)
Decks:
    !byte C_GOBLIN_LEADER,C_GOBLIN_LEADER+5,C_GOBLIN_LEADER+10,C_GOBLIN_LEADER+15,C_GOBLIN_LEADER+20,C_GOBLIN_LEADER+25,C_GOBLIN_LEADER+30,C_GOBLIN_LEADER+35
    !byte C_POLY_LEADER,C_POLY_LEADER+5,C_POLY_LEADER+10,C_POLY_LEADER+15,C_POLY_LEADER+20,C_POLY_LEADER+25,C_POLY_LEADER+30,C_POLY_LEADER+35
    !byte C_CANDY_LEADER,C_CANDY_LEADER+5,C_CANDY_LEADER+10,C_CANDY_LEADER+15,C_CANDY_LEADER+20,C_CANDY_LEADER+25,C_CANDY_LEADER+30,C_CANDY_LEADER+35
    !byte C_SOAP_LEADER,C_SOAP_LEADER+5,C_SOAP_LEADER+10,C_SOAP_LEADER+15,C_SOAP_LEADER+20,C_SOAP_LEADER+25,C_SOAP_LEADER+30,C_SOAP_LEADER+35


;----------------------------------------------------------------------------
; GLYPHS
;----------------------------------------------------------------------------

; glyphs, 3x3 bytes per glyph = max 252/9 = 28 glyphs
; NOTE since 256 does not divide by 9, glyph offset modulo 256 (i.e. the low byte) can be used to index a hash of high bytes
;      i.e. hash[low]=high which compresses really well since it just repeats 0 2 4 6 8 1 3 5 7
GlyphData:
    G_LEGND_GOBLIN=*-GlyphData
    !byte 73,104,85
    !byte 215,215,117
    !byte 81,73,41
    G_2=*-GlyphData
    !byte 73,104,85
    !byte 118,215,215
    !byte 40,58,87
    G_3=*-GlyphData
    !byte 73,104,104
    !byte 85,108,108
    !byte 40,58,87
    G_4=*-GlyphData
    !byte 104,104,85
    !byte 251,251,117
    !byte 87,58,29
    G_5=*-GlyphData
    !byte 78,77,85
    !byte 87,87,62
    !byte 95,58,62
    G_6=*-GlyphData
    !byte 32,85,73
    !byte 233,160,93
    !byte 95,105,32
    G_7=*-GlyphData
    !byte 32,92,92
    !byte 92,102,163
    !byte 163,163,102
    G_8=*-GlyphData
    !byte 32,30,32
    !byte 32,66,32
    !byte 233,226,223
    G_LEGND_POLY=*-GlyphData
    !byte 206,160,205
    !byte 160,182,160
    !byte 192,159,192
    G_10=*-GlyphData
    !byte 100,92,100
    !byte 101,32,103
    !byte 77,100,78
    G_11=*-GlyphData
    !byte 32,107,104
    !byte 32,160,32
    !byte 32,160,32
    G_12=*-GlyphData
    !byte 32,32,233
    !byte 32,78,32
    !byte 78,32,32
    G_13=*-GlyphData
    !byte 225,229,97
    !byte 103,212,101
    !byte 103,212,101
    G_14=*-GlyphData
    !byte 46,32,32
    !byte 160,46,46
    !byte 160,160,160
    G_15=*-GlyphData
    !byte 85,64,73
    !byte 67,67,67
    !byte 74,64,75
    G_16=*-GlyphData
    !byte 77,77,32
    !byte 233,236,223
    !byte 95,105,95
    G_LEGND_CANDY=*-GlyphData
    !byte 223,98,233
    !byte 160,160,160
    !byte 105,226,95
    G_18=*-GlyphData
    !byte 92,92,92
    !byte 87,87,102
    !byte 226,75,102
    G_19=*-GlyphData
    !byte 77,78,223
    !byte 78,78,78
    !byte 95,78,77
    G_20=*-GlyphData
    !byte 83,32,83
    !byte 83,83,83
    !byte 32,83,32
    G_21=*-GlyphData
    !byte 85,68,73
    !byte 71,87,72
    !byte 74,70,75
    G_22=*-GlyphData
    !byte 1,2,3
    !byte 4,5,6
    !byte 7,8,78
    G_23=*-GlyphData
    !byte 240,242,238
    !byte 235,219,243
    !byte 237,241,253
    ; G_24=*-GlyphData
    ; !byte 95,95,32
    ; !byte 233,215,223
    ; !byte 95,206,160
    G_LEGND_SOAP=*-GlyphData
    !byte 255,251,252
    !byte 160,160,160
    !byte 124,226,126
    G_26=*-GlyphData
    !byte 230,123,102
    !byte 230,102,230
    !byte 102,230,102
    G_27=*-GlyphData
    !byte 32,85,73
    !byte 233,160,93
    !byte 95,105,32
    G_28=*-GlyphData
    !byte 233,215,233
    !byte 233,160,160
    !byte 120,120,120
    ; G_29=*-GlyphData
    ; !byte 32,32,32
    ; !byte 32,32,32
    ; !byte 32,32,32
    ; G_30=*-GlyphData
    ; !byte 32,32,32
    ; !byte 32,32,32
    ; !byte 32,32,32
    ; G_31=*-GlyphData
    ; !byte 32,32,32
    ; !byte 32,32,32
    ; !byte 32,32,32
    G_WANNABE=*-GlyphData
    !byte 127,98,126
    !byte 17,17,97
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
    E_ALL_GAIN11=*-TextData ; All other cards of the same suit gain +1/+1
    E_INTERNAL_ALLGAIN11=*-TextData+1
    !byte M_ALL,M_SUIT,M_GAIN11,0
    E_READY=*-TextData ; Card has no summoning sickness
    !byte M_READY,0
    T_YOUR_OPPONENT_IS=*-TextData
    !byte M_YOUR,M_OPPONENT,M_IS
    T_OPPONENT_NAME=*-TextData
    !byte M_OPPONENT_NAME,0
    T_PICK_DECK=*-TextData
    !byte M_2SPACES,M_PICK,M_A,M_DECK,M_2SPACES,0
    T_SUIT_DECK=*-TextData
    !byte M_SUIT,M_DECK,0
    T_END=*-TextData
    !byte M_END,0
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
    M_YOUR        =*-MacroData+1 : !scr "you",'r'+$80
    M_OPPONENT    =*-MacroData+1 : !scr "opponen",'t'+$80
    M_IS          =*-MacroData+1 : !scr "i",'s'+$80
    M_OPPONENT_NAME=*-MacroData+1
opponent_name1:                    !scr "a."        ; SELF-MODIFIED
opponent_name2:                    !scr "p",'.'+$80 ; SELF-MODIFIED
    M_2SPACES     =*-MacroData+1 : !scr " ",' '+$80
    M_PICK        =*-MacroData+1 : !scr "pic",'k'+$80
    M_A           =*-MacroData+1 : !scr 'a'+$80
    M_DECK        =*-MacroData+1 : !scr "dec",'k'+$80
    M_END         =*-MacroData+1 : !scr "en",'d'+$80
    M_ALL         =*-MacroData+1 : !scr "al",'l'+$80
    M_GAIN11      =*-MacroData+1 : !scr "gain ",78,'1',83,'1'+$80
    M_READY       =*-MacroData+1 : !scr "ready",'.'+$80
!if *-MacroData >= $FF { !error "Out of MacroData memory" }

SIZEOF_TEXT=*-TextData

AINames:
    !scr "bd","jt","mg","rh"


;----------------------------------------------------------------------------
; FRAMES
;----------------------------------------------------------------------------

; DrawF_Frame source data, use $60/96 to skip over a char
FrameData:
    FRAME_CARD=*-FrameData              ; Full 5x6 card frame without top decorations
    FRAME2_CARD=*-FrameData             ; 5x2 top of card frame without top decorations
frame_TYPE: !byte 79
frame_COST: !byte 119
    !byte 119,119,80
    !byte 116,96,96,96,106
    !byte 116,96,96,96,106
    !byte 116,96,96,96,106
    !byte 76,111,111,111,122
    !byte 206
frame_ATK: !byte $B0
    !byte 160,211
frame_DEF: !byte $B0
    ; Full 5x6 card back
    FRAME_CARDBACK=*-FrameData          ; Full 5x6 card back
    !byte 236,192,192,192,251
    !byte 194,102,102,102,194
    !byte 194,102,102,102,194
    !byte 194,102,102,102,194
    FRAME2_CARDBACK=*-FrameData         ; 5x2 bottom of card back
    !byte 194,102,102,102,194
    !byte 252,192,192,192,254
    FRAME_SHIELDED=*-FrameData          ; 5x6 shielded overlay
    !byte 96,42,42,42,96
    !byte 42,96,96,96,42
    !byte 42,96,96,96,42
    !byte 42,96,96,96,42
    !byte 96,42,42,42,96
    !byte 96,96,96,96,96


;----------------------------------------------------------------------------
; RUN TIME DATA
;----------------------------------------------------------------------------

; Effect Queue (SoA)
; Put next effect at index EfQPtr and call EffectAdded
EQueueEffect:
    !fill MAX_EFFECT_QUEUE,0
EQueueSource:
    !fill MAX_EFFECT_QUEUE,0
EQueueParam:
    !fill MAX_EFFECT_QUEUE,0


;----------------------------------------------------------------------------
; LOGO INTRO SCREEN
;----------------------------------------------------------------------------

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
            dec .logoptr-2              ; remove #81 ball from screen

            ; fill everything but the logo blue
--          ldx #0
            lda #BLUE-DEBUG
            .screenptr=*+1
-           ldy SCREEN,x
---         cmp $d012 ; slow down
            bne ---
            iny
            cpy #81+1                   ; compare with ball+1
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
+           cmp #$E8                    ; end of screen
            bne --
            jsr .putalexander

            jsr DebounceJoystick
-           jsr ReadJoystick
            beq -
            ; fall through

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
            lda #COL_LEGEND
-           sta $d884,x
            dex
            bpl -
            rts

.putalexander:
            ldx #28-1
-           lda Alexander,x
            eor #$AA
            sta SCREEN+21*40+(40-28)/2,x
            lda #COL_LEGEND
            sta $D800+21*40+(40-28)/2,x
            dex
            bpl -
            rts

Alexander:
            !scrxor $AA, "a game by alexander paalvast" ; 28 bytes
; TODO put at right place on screen
            !scr "twain pain games" ; 16 bytes

.drawlogo:
            ldx #0
--          ldy #10 ; 10 pixels = 8 from the byte and 2 empty
            .logosrc = *+1
-           asl logo,x
            bcc +
            lda #81                     ; ball
            .logoptr = *+1
            sta SCREEN+1+5*40
            sta $d800+1+5*40
---         cmp $d012                   ; slow down
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


;----------------------------------------------------------------------------
!byte 0 ; DUMMY to show where we are in report
!if * >= $1800 { !error "Out of memory - start compressing" }
