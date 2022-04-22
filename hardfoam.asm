; HARD FOAM - a 4K compressed card game
; Developed for the https://ausretrogamer.com/2022-reset64-4kb-craptastic-game-competition

; TODO: SID
; TODO: don't draw ATK/DEF on Spell cards
; TODO: on play select place on table
; TODO: on play select target on table (own or opponent)
; TODO: 14+1 free deck selector
; TODO: decide on logo (adds at least 200 bytes packed)
; TODO: experimenting with _Draw vs specific functions showed 2x speedup (maybe more)

!ifndef DEBUG {DEBUG=0}
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
CHR_ATK=78 ; /
CHR_DEF=83 ; heart
CHR_LIFE=83 ; heart
;
HAND_CARDWIDTH=4
TABLE_CARDWIDTH=5
MAX_EFFECT_QUEUE=20
AI_ATTACKS=50

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
!addr EfQTmp=$0B ; reuse Tmp2
!addr Tmp3=$0C
!addr TmpText=$0D
!addr Card=$0E
!addr Joystick=$0F
; Player Data (consecutive) (SIZEOF_PD=64 bytes)
!addr PlayerData=$10
    PD_LIFE=0       ; 0 .. 10
    PD_ENERGY=1     ; $30 .. $39
    ; fixed '/' character in between
    PD_MAXENERGY=3  ; $30 .. $39
    PD_REMAIN=4     ; DECKSIZE-1 .. 0
    PD_HAND=5       ; 7 bytes card# or $FF=no card (left filled)
     MAX_HAND=6     ; max #cards in hand
    PD_TABLE=12     ; 20 bytes 5*4 bytes (card#,atk,def,status)
      TD_CARD=0     ; card# $FF=no card
      TD_ATK=1      ; attack
      TD_DEF=2      ; defense
      TD_STATUS=3   ; status bits: 1=tapped, 2=guard, 4=shield
      STATUS_TAPPED=1
      STATUS_GUARD=2
      STATUS_SHIELD=4
      SIZEOF_TD=4
     MAX_TABLE=5    ; max #cards on table
    PD_DECK=32      ; 32 bytes (card#)
SIZEOF_PD=64
!addr Index=$50     ; loop/selection index
!addr MaxIndex=$51  ; <MaxIndex
!addr SelectorIndex=$52 ; Index used for DrawCardSelect/ClearCardSelect
!addr SelectorIndexBackup=$53
!addr TableIdx=$54  ; loop index for DrawTable
!addr InsertingPos=$55 ; place to insert card during DrawTable ($FF=disable)
!addr InsertingCard=$56 ; card to show during DrawTable
!addr EfQPtr=$57    ; next place in Effect Queue
!addr EfSource=$58  ; Effect source (Table-card or 0 in case of spell)
!addr EfParam=$59   ; Parameter for effect (table or card to apply effect to)
!addr FxCard=$5A    ; FX Table-card
!addr FxScrOff=$5B  ; FX screen offset
!addr FxPtr=$5C     ; FX current table offset
!addr FxLoop=$5D    ; FX loop counter temp data used during FX
!addr AiAttacks=$5E ; #attacks the AI tries
!addr StatusMask=$5F; combined statuses of all cards on a table
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

!macro SKIP2 {
    !byte $2C ; BIT skip next 2 bytes
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
DrawF_CharCol:
            lda CharCol
            sta (_ColorPos),y
+           jmp _DrawEnd

; draws glyph in SuitCol at Cursor + Y + 1 (clobbers A,X)
DrawF_Glyph:
            iny
            lda GlyphData,x
            inx
            sta (_CursorPos),y
            lda SuitCol
            sta (_ColorPos),y
            dey
            jmp _DrawEnd

; draws space in SuitCol at Cursor + Y (clobbers A)
DrawF_ClearSuitCol:
            lda #CHR_SPACE
DrawF_ASuitCol: ; (clobbers A)
            sta (_CursorPos),y
            lda SuitCol
            sta (_ColorPos),y
            jmp _DrawEnd

!if (>DrawF_Frame != >DrawF_Frame) { !error "All DrawF functions must be in same page" }


;----------------------------------------------------------------------------
; CHARACTER DRAWING
;----------------------------------------------------------------------------

; configures Draw function with X=config and Draws with A=srcoffset, Y=0(dstoffset) (clobbers A,X,Y)
Draw:
            ldy #0
; configures Draw function with X=config and Draws with A=srcoffset, Y=dstoffset (clobbers A,X,Y)
DrawWithOffset:
            pha
            lda ConfigData,x
            sta <.drawfptr
            lda ConfigData+1,x
            sta <.drawwidth
            lda ConfigData+2,x
            sta <.drawheight
            pla
            tax
            jmp _Draw

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
+           sta (_CursorPos),y
            lda SuitCol
            sta (_ColorPos),y
            iny
            inx
            bne -
            ldx TmpText
            lda TextData+1,x            ; lookahead
            beq ++                      ; text ends directly with 0
            lda #CHR_SPACE
            sta (_CursorPos),y
            lda SuitCol
            sta (_ColorPos),y
            iny
            inx
            bne --
++          rts

SuitTextData:
    !byte M_GOBLIN,M_POLYSTYRENE,M_CANDY,M_SOAP

; clears the two lower lines (clobbers A,X)
ClearLowerLines: ; 14 bytes
            lda #CHR_SPACE
            ldx #38
-           sta SCREEN+21*40,x
            sta SCREEN+22*40,x
            dex
            bpl -
            rts

; clears two lines upper lines (clobbers A,X)
ClearUpperLines: ; 14 bytes
            lda #CHR_SPACE
            ldx #38
-           sta SCREEN+2*40,x
            sta SCREEN+3*40,x
            dex
            bpl -
            rts

; wipes the top and bottom of the screen completely (clobbers A,X)
ClearAll: ; 20 bytes
            lda #CHR_SPACE
            ldx #5*40
-           sta SCREEN-1,x
            sta SCREEN-1+5*40,x
            sta SCREEN-1+15*40,x
            sta SCREEN-1+20*40,x
            dex
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
            lda #10
            sec
            sbc AIData+PD_LIFE
            tax
            lda #COL_HEALTH_ON
            sta CharCol
            ldy #<(SCREEN+39)
            lda #>(SCREEN+39)
            jsr .healthbar
            ldx PlayerData+PD_LIFE
            lda #COL_HEALTH_OFF
            sta CharCol
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
+           lda #CHR_LIFE
            ldy #0
            sta (_CursorPos),y
            lda CharCol
            sta (_ColorPos),y
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
-           jmp DrawF_Frame             ; (1) change low byte of ptr to other routine
_DrawEnd:
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
            lda #CHR_SPACE
            ldy CardSelectorOffsets,x
            sta (_CursorPos),y
            inx
            ldy CardSelectorOffsets,x
            sta (_CursorPos),y
            rts

; draws selection symbols around a card at Cursor + Index*6 (clobbers A,X,Y) sets SelectorIndex to Index
; - cursor is assumed to be top-left of card deck
DrawSelector:
            ldx Index
            stx SelectorIndex
            lda #'>'
            ldy CardSelectorOffsets,x
            sta (_CursorPos),y
            lda #COL_SELECTED
            sta (_ColorPos),y
            inx
            lda #'<'
            ldy CardSelectorOffsets,x
            sta (_CursorPos),y
            lda #COL_SELECTED
            sta (_ColorPos),y
            rts

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
            ldy #<(SCREEN+15*40)
            lda #>(SCREEN+15*40)
            jsr SetCursorDrawCardBack

;---------------
; PLAYER'S TURN
;---------------

NextPlayerRound:
            ; pull next card
            jsr PullPlayerDeckCard
            jsr DrawPlayerHand

            ; restore energy for player
            ldx #PlayerData
            jsr Energize
            jsr DrawCounters

            ldy #S_YOURTURN
            lda #COL_PLAIN
            jsr PlayScroll
            ldy #50
-           +WaitVBL($43)
            dey
            bne -
            jsr PlayScrollEmpty

            ; select from hand
            lda #0                      ; 0..MAX_HAND-1=card, MAX_HAND=END
            sta SelectorIndex
            sta SelectorIndexBackup

            ; draw selected card for player (or card back)
.redraw:    ldy #<(SCREEN+15*40)
            lda #>(SCREEN+15*40)
            jsr SetCursor
            +WaitVBL($43)
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
            pha                         ; backup X
            jsr DrawCard
            pla
            tax                         ; restore X
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
            beq .can_end
+           ldy PlayerData+PD_HAND,x
            cpy #$FF                    ; is there a card?
            beq .no_energy              ; no card -> handle as if no energy
            lda Cards+CARD_LTSC,y
            and #%01000000              ; LTSSCCCC T=Type(0=Spell/1=Monster)
            beq +
            lda PlayerData+PD_TABLE+SIZEOF_TD*(MAX_TABLE-1)+TD_CARD ; last table-card
            cmp #$FF                    ; still open?
            bne .no_energy              ; table full
+           lda Cards+CARD_LTSC,y
            and #%00001111              ; LTSSCCCC
            ora #$30                    ; $30..$39 as energy
            cmp PlayerData+PD_ENERGY
            beq .can_afford
            bcc .can_afford
.no_energy: lda #CHR_NO_PLAY
            +SKIP2
.can_end:   lda #CHR_ENDTURN
            +SKIP2
.can_afford:lda #CHR_PLAY
++          ldy HandSelectorOffsets,x
            sta (_CursorPos),y
            lda SuitCol
            sta (_ColorPos),y

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
+           cmp #%11111110              ; UP
            bne +
            lda PlayerData+PD_TABLE+TD_CARD ; first table-card
            cmp #$FF                    ; still open?
            beq ++                      ; no attack possible
            jsr DrawPlayerHand          ; remove cursor
            jmp PlayerAttack
+           cmp #%11101111              ; FIRE
            bne ++
            ldx SelectorIndex
            ldy HandSelectorOffsets,x

            ; end of turn?
            lda #CHR_ENDTURN
            cmp (_CursorPos),y
            bne +                       ; nope
            ; end of turn
            ldx #PlayerData+PD_TABLE
            jsr UntapTable
            jsr DrawPlayerTable
            jmp NextAIRound

            ; play card?
+           lda #CHR_PLAY
            cmp (_CursorPos),y
            bne ++                      ; nope (not possible)
;}
            ; TODO pick destination on table (LEFT/RIGHT and DOWN to cancel), and if required, pick target
            jsr CastPlayerCard
RunPlayerAction:
            ; redraw screen
            +WaitVBL($83)
            jsr DrawCounters
            jsr DrawPlayerHand
            jsr DrawPlayerTable
            jsr ClearLowerLines
            jsr PlayScrollCard
            ; run all queued effects
.effects1:  jsr RunEffect
            jsr DrawCounters
            jsr DrawPlayerTable
            ldy EfQPtr
            bne .effects1
            jsr QueueDeaths
.effects2:  jsr RunEffect
            ldy EfQPtr
            bne .effects2
            jsr CleanupDeaths
            jsr DrawPlayerTable
            jsr DrawAITable
            jsr PlayScrollEmpty
            ; check player deaths after playing a card
            lda PlayerData+PD_LIFE
            bne .d1
            lda #ORANGE
            ldy #S_LOSE
            jmp GameOver
.d1:        lda AIData+PD_LIFE
            bne ++
            lda #GREEN
            ldy #S_WIN
            jmp GameOver

++          jmp .redraw

HandSelectorOffsets:
    !for i,0,MAX_HAND { !byte 10 + i * HAND_CARDWIDTH }

; player selects attacker
PlayerAttack:
            ldy #<(SCREEN+15*40)
            lda #>(SCREEN+15*40)
            jsr SetCursorDrawCardBack
.attack3:   ldx #MAX_TABLE              ; TODO calculate actual #cards on table (this works, but needs to check for empty)
            jsr SetMaxIndexX0
.attack2:   ; draw text corresponding to target
            jsr ClearLowerLines
            ldx Index
            ldy PlayerTableCardOffsets,x
            ldx TD_CARD,y
            cpx #$FF
            beq +                       ; empty
            ldy #<(SCREEN+21*40)
            lda #>(SCREEN+21*40)
            jsr SetCursorDrawCardText
+           ldy #<(SCREEN+15*40+(40-24)/2)
            lda #>(SCREEN+15*40+(40-24)/2)
            jsr SetCursor
            jsr SelectCard              ; moves LEFT/RIGHT
            lda Joystick                ; 111FRLDU
            cmp #%11101111              ; FIRE
            bne +
            ; can this card attack?
            ldy Index
            ldx PlayerTableCardOffsets,y; X=table-card source attacker
            lda TD_STATUS,x
            and #STATUS_TAPPED
            bne .attack2                ; tapped card can't attack
            ; Attack
            lda AIData+PD_TABLE+TD_CARD ; is there a table-card?
            cmp #$FF                    ; still open?
            bne .target
            ; there are no AI cards, so attack AI player directly
            lda TD_CARD,x
            sta Card
            lda TD_STATUS,x
            ora #STATUS_TAPPED
            sta TD_STATUS,x             ; X=table-card source attacker
            ldy #AIData                 ; Y=player to attack
            lda #E_INT_ATTACKPLAYER
            jsr QueueEffect
            jmp RunPlayerAction
            ; otherwise, select AI Card target to attack
.target:    txa
            pha                         ; backup X
            jsr PlayerPickTarget
            pla
            tax                         ; restore X
            cpy #0                      ; Y=card to attack
            beq .attack3                ; aborted
            lda TD_CARD,x
            sta Card
            lda TD_STATUS,x
            ora #STATUS_TAPPED
            sta TD_STATUS,x             ; X=table-card source attacker
            lda #E_INT_ATTACK
            jsr QueueEffect
            jmp RunPlayerAction
+           cmp #%11111101              ; DOWN
            bne .attack2
            lda SelectorIndexBackup     ; restore selector on lower screen
            sta SelectorIndex
            jmp .redraw

; player picks target from AI cards <> empty! (clobbers A,X,Y,Index) returns Y=selected table-card or 0 if none
PlayerPickTarget: ; TODO also usable to just browse AI cards
            ldx #AIData+PD_TABLE
            jsr CalculateStatusMask
            ldx #MAX_TABLE              ; TODO calculate actual #cards on table (this works, but needs to check for empty)
            jsr SetMaxIndexX0
.pick2:     jsr ClearUpperLines
            ldx Index
            ldy AITableCardOffsets,x
            ldx TD_CARD,y
            cpx #$FF
            beq +                       ; empty
            ldy #<(SCREEN+2*40)
            lda #>(SCREEN+2*40)
            jsr SetCursorDrawCardText
+           ldy #<(SCREEN+4*40+(40-24)/2)
            lda #>(SCREEN+4*40+(40-24)/2)
            jsr SetCursor
            jsr SelectCard              ; moves LEFT/RIGHT
            lda Joystick                ; 111FRLDU
            cmp #%11101111              ; FIRE
            bne +
            ; target confirmed
            ldx Index
            ldy AITableCardOffsets,x    ; selected table-card in Y
            lda TD_CARD,y
            cmp #$FF
            beq .pick2                  ; empty, retry
            lda StatusMask
            and #STATUS_GUARD
            beq ++                      ; no Guards so fine
            lda TD_STATUS,y
            and #STATUS_GUARD
            beq .pick2                  ; must pick a Guard, retry
++          jmp ClearUpperLines
+           cmp #%11111101              ; DOWN
            bne .pick2
            ldy #0                      ; return 0 in Y in case of abort
            jmp ClearUpperLines


;-----------
; AI'S TURN
;-----------

NextAIRound:
            ; pull next card
            jsr PullAIDeckCard
            jsr DrawAIHand

            ; restore energy for player
            ldx #AIData
            jsr Energize
            jsr DrawCounters

            ldy #S_OPPONENTSTURN
            lda #COL_PLAIN
            jsr PlayScroll
            ldy #AI_ATTACKS
            sty AiAttacks
-           +WaitVBL($43)
            dey
            bne -
            jsr PlayScrollEmpty

            ; AI turn: while possible (room on table, possible target), play random cards from hand
.ai_castmore:
            jsr Random
            ;and #$0F                    ; 0..15
            sta Tmp1                    ; nr of castable cards to skip
            sta Tmp2
--          ldx #0
-           ldy AIData+PD_HAND,x
            cpy #$FF
            beq ++
            lda Cards+CARD_LTSC,y
            and #%00001111              ; LTSSCCCC
            ora #$30                    ; $30..$39 as energy
            cmp AIData+PD_ENERGY
            beq .ai_canuse
            bcs +
.ai_canuse: ; verify that table has room for monster card to cast
            lda Cards+CARD_LTSC,y
            and #%01000000              ; LTSSCCCC T=Type(0=Spell/1=Monster)
            beq .ai_canuse2             ; spells can always be cast
            lda AIData+PD_TABLE+SIZEOF_TD*(MAX_TABLE-1)+TD_CARD ; last table-card
            cmp #$FF                    ; still open?
            bne +                       ; table full
.ai_canuse2:dec Tmp1
            bpl +
            ; castable card found
            jsr CastAICard
RunAIAction:
            +WaitVBL($83)
            jsr DrawCounters
            jsr DrawAIHand
            jsr DrawAITable
            jsr PlayScrollCard
            ; run all queued effects
.effects3:  jsr RunEffect
            jsr DrawCounters
            jsr DrawAITable
            ldy EfQPtr
            bne .effects3
            jsr QueueDeaths
.effects4:  jsr RunEffect
            ldy EfQPtr
            bne .effects4
            jsr CleanupDeaths
            jsr DrawPlayerTable
            jsr DrawAITable
            jsr PlayScrollEmpty
            ; check player deaths after playing a card
            lda PlayerData+PD_LIFE
            bne .d2
            ldy #S_LOSE
            jmp GameOver
.d2:        lda AIData+PD_LIFE
            bne .ai_castmore
            ldy #S_WIN
            jmp GameOver
+           inx
            cpx #MAX_HAND
            bne -
++          lda Tmp1
            cmp Tmp2                    ; are any cards being considered?
            bne --                      ; yes, so just retry
            ; no castable cards found

            ; AI turn: select a random table card, and if it exists and can attack, attack with it
            ldx #PlayerData+PD_TABLE
            jsr CalculateStatusMask
.rndcard:   jsr Random
            cmp #MAX_TABLE
            bcs .rndcard
            tay
            ldx AITableCardOffsets,y
            lda TD_CARD,x
            cmp #$FF
            beq .ai_nextatk             ; empty can't attack
            lda TD_STATUS,x
            and #STATUS_TAPPED
            bne .ai_nextatk             ; tapped card can't attack
            lda TD_ATK,x
            beq .ai_nextatk             ; don't attack when ATK=0 (that will only hurt)
            ; Attack
.ai_attack: lda PlayerData+PD_TABLE+TD_CARD ; is there a table-card?
            cmp #$FF                    ; still open?
            bne .target2
            ; there are no Player cards, so attack Player directly
            lda TD_CARD,x
            sta Card
            lda TD_STATUS,x
            ora #STATUS_TAPPED
            sta TD_STATUS,x             ; X=table-card source attacker
            ldy #PlayerData             ; Y=player to attack
            lda #E_INT_ATTACKPLAYER
            jsr QueueEffect
            jmp RunAIAction
            ; otherwise, select Player Card target to attack
.target2:   ; pick random card to attack
.rndtarget: jsr Random
            cmp #MAX_TABLE
            bcs .rndtarget
            tay
            lda PlayerTableCardOffsets,y
            tay
            lda TD_CARD,y               ; Y=card to attack
            cmp #$FF
            beq .rndtarget
            lda StatusMask
            and #STATUS_GUARD
            beq +                       ; no Guards so fine
            lda TD_STATUS,y
            and #STATUS_GUARD
            beq .rndtarget              ; must pick a Guard, retry
+           lda TD_CARD,x
            sta Card
            lda TD_STATUS,x
            ora #STATUS_TAPPED
            sta TD_STATUS,x             ; X=table-card source attacker
            lda #E_INT_ATTACK
            jsr QueueEffect
            jmp RunAIAction
.ai_nextatk:
            dec AiAttacks
            bne .rndcard                ; try more cards on table

            ; end of turn
.ai_done:   ldx #AIData+PD_TABLE
            jsr UntapTable
            jsr DrawAITable
            jmp NextPlayerRound

;-----------
; GAME OVER
;-----------

; Y=scroll text
GameOver:
            jsr PlayScroll
-           jsr ReadJoystick
            bne -
-           jsr ReadJoystick
            beq -
            jsr PlayScrollEmpty
            jmp Start


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

; cast card #X from hand of Player (clobbers A,X,Y) TODO card index on table + possible target
CastPlayerCard:
            ldy PlayerData+PD_HAND,x    ; card# in Y
            sty Card
            ; remove card by shifting cards X..MAX_HAND-1 left
-           cpx #MAX_HAND-1             ; 0..MAX_HAND-1
            beq +
            lda PlayerData+PD_HAND+1,x
            sta PlayerData+PD_HAND,x
            inx
            bne -                       ; "always"
+           lda #$FF
            sta PlayerData+PD_HAND,x    ; wipe last card
            ldx #PlayerData
            lda Cards+CARD_LTSC,y
            and #%00001111              ; LTSSCCCC
            jsr DecreaseEnergy
            lda Cards+CARD_LTSC,y
            and #%01000000              ; LTSSCCCC T=Type(0=Spell/1=Monster)
            beq .spell
            ; put monster card on table
            ldx #PlayerData+PD_TABLE    ; table-card location on table
            jsr PutCardOnTable
            lda #E_INT_DROPCARD         ; drop card Y at X (table-card or Player)
            jmp QueueEffect
.spell:     lda Cards+CARD_EFFECT,y
            jmp QueueEffect

; cast card #X from hand of AI (clobbers A,X,Y) TODO card index on table + possible target
CastAICard:
            ldy AIData+PD_HAND,x        ; card# in Y
            sty Card
            ; remove card by shifting cards X..MAX_HAND-1 left
-           cpx #MAX_HAND-1             ; 0..MAX_HAND-1
            beq +
            lda AIData+PD_HAND+1,x
            sta AIData+PD_HAND,x
            inx
            bne -                       ; "always"
+           lda #$FF
            sta AIData+PD_HAND,x        ; wipe last card
            ldx #AIData
            lda Cards+CARD_LTSC,y
            and #%00001111              ; LTSSCCCC
            jsr DecreaseEnergy
            lda Cards+CARD_LTSC,y
            and #%01000000              ; LTSSCCCC T=Type(0=Spell/1=Monster)
            beq .spell2
            ; put monster card on table
            ldx #AIData+PD_TABLE        ; table-card space on table
            jsr PutCardOnTable
            lda #E_INT_DROPCARD
            jmp QueueEffect
.spell2:    lda Cards+CARD_EFFECT,y
            jmp QueueEffect

; insert card Y on table-card X (clobbers A,Tmp1)
; Note that this blindly assumes there's room on the table!
PutCardOnTable:
            stx Tmp1
            ; make room for card on table
            txa
            and #$80                    ; make it work with both Players
            ora #PlayerData+PD_TABLE+(MAX_TABLE-1)*SIZEOF_TD-1 ; last byte of 2nd last card
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

PlayerTableCardOffsets:
            !for i,0,MAX_TABLE-1 { !byte PlayerData+PD_TABLE + i * SIZEOF_TD }
AITableCardOffsets:
            !for i,0,MAX_TABLE-1 { !byte AIData+PD_TABLE + i * SIZEOF_TD }

; untap all cards on table in X (clobbers A,X)
UntapTable:
-           lda TD_CARD,x
            cmp #$FF
            beq +
            lda TD_STATUS,x
            and #255-STATUS_TAPPED
            sta TD_STATUS,x
+           txa
            clc
            adc #SIZEOF_TD
            tax
            and #$7F                    ; make it work with both Players
            cmp #PlayerData+PD_TABLE+MAX_TABLE*SIZEOF_TD
            bne -
            rts

; check cards and players for health=0 and queue death effect (clobbers A,X)
QueueDeaths:
            ldx #PlayerData+PD_TABLE
            jsr .qdeaths
            ldx #AIData+PD_TABLE
.qdeaths:   lda TD_CARD,x
            cmp #$FF
            beq +
            lda TD_DEF,x
            bne +
            ; card is dead
            lda #E_INT_DEADCARD
            jsr QueueEffect
+           txa
            clc
            adc #SIZEOF_TD
            tax
            and #$7F                    ; make it work with both Players
            cmp #PlayerData+PD_TABLE+MAX_TABLE*SIZEOF_TD
            bne .qdeaths
            rts

; clean out cards with health=0 of table X (clobbers A,X,Y)
CleanupDeaths:
            ldx #PlayerData+PD_TABLE
            jsr .cleanup
            ldx #AIData+PD_TABLE
.cleanup:   txa                         ; X=source
            tay                         ; Y=dest
-           lda TD_CARD,x
            cmp #$FF
            beq +                       ; always copy empty card
            lda TD_DEF,x
            beq .clskip
            ; copy card
            lda TD_CARD,x
+           sta TD_CARD,y
            iny
            lda TD_CARD+1,x
            sta TD_CARD,y
            iny
            lda TD_CARD+2,x
            sta TD_CARD,y
            iny
            lda TD_CARD+3,x
            sta TD_CARD,y
            iny
.clskip:    txa
            clc
            adc #SIZEOF_TD
            tax                         ; X=X+SIZEOF_TD
            and #$7F                    ; make it work with both Players
            cmp #PlayerData+PD_TABLE+MAX_TABLE*SIZEOF_TD
            bne -
            ; fill remainder
            tya
            ldy #$FF
            bne ++                      ; always
-           sty TD_CARD,x
            txa
            clc
            adc #SIZEOF_TD
++          tax                         ; X=X+SIZEOF_TD
            and #$7F                    ; make it work with both Players
            cmp #PlayerData+PD_TABLE+MAX_TABLE*SIZEOF_TD
            bne -
            rts

; calculate combined StatusMask for all cards on table in X (clobbers A,X)
CalculateStatusMask:
            lda #0
            sta StatusMask
-           lda TD_CARD,x
            cmp #$FF
            beq +
            lda TD_STATUS,x
            ora StatusMask
            sta StatusMask
+           txa
            clc
            adc #SIZEOF_TD
            tax
            and #$7F                    ; make it work with both Players
            cmp #PlayerData+PD_TABLE+MAX_TABLE*SIZEOF_TD
            bne -
            rts

; picks random table-card of Player in A (clobbers A,X,Tmp1,Tmp2) returns table-card in X
PickRandomTargetInX:
            ora #PD_TABLE
            sta Tmp1                    ; backup table
            jsr Random                  ; 0-255
            sta Tmp2                    ; nr of cards to skip (can be 0)
            ldx Tmp1
            lda TD_CARD,x
            cmp #$FF
            beq .stealrts3              ; abort
--          ldx Tmp1
-           lda TD_CARD,x
            cmp #$FF
            beq --                      ; restart loop
            dec Tmp2
            beq .stealrts3              ; found one
            txa
            clc
            adc #SIZEOF_TD
            tax
            and #$7F                    ; make it work with both Players
            cmp #PlayerData+PD_TABLE+MAX_TABLE*SIZEOF_TD
            bne -
            beq --                      ; always


;----------------------------------------------------------------------------
; EFFECTS
;----------------------------------------------------------------------------

; Queue effect A with Source X and Param Y (clobbers A,EfQTmp)
QueueEffect:
            sty EfQTmp
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
            lda EfQTmp
            sta EQueueParam,y
            tay
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
; untap Source
Effect_Untap:
            ldx EfSource                ; table-card
            lda TD_STATUS,x
            and #255-STATUS_TAPPED
            sta TD_STATUS,x
            lda #FX_UNTAP
            jmp PlayFX

+           cmp #E_GUARD
            bne +
; add guard status
Effect_Guard:
            ldx EfSource                ; table-card
            lda TD_STATUS,x
            ora #STATUS_GUARD
            sta TD_STATUS,x
            lda #FX_GUARD
            jmp PlayFX

+           cmp #E_ALL_GAIN_A1D1
            bne +
Effect_AuraAllGain11:
; for each card on table (not Source) that matches Suit of Source post effect
            sta EfParam
            ldx EfSource                ; table-card
            ldy TD_CARD,x
            lda Cards+CARD_LTSC,y
            and #%00110000              ; LTSSCCCC SS=Suit(0,1,2,3)
            sta Tmp3                    ; Suit
            txa
            and #$80
            ora #PlayerData+PD_TABLE
            tax                         ; first table-card
            cpx EfSource
            beq ++                      ; skip self
-           ldy TD_CARD,x
            cpy #$FF                    ; empty
            beq ++
            lda Cards+CARD_LTSC,y
            and #%00110000              ; LTSSCCCC SS=Suit(0,1,2,3)
            cmp Tmp3
            bne ++                      ; skip wrong suit
            lda #E_INT_GAIN_A1D1
            jsr QueueEffect
++          txa
            clc
            adc #SIZEOF_TD
            tax
            and #$7F                    ; make it work with both Players
            cmp #PlayerData+PD_TABLE+MAX_TABLE*SIZEOF_TD
            bne -
.stealrts4: rts

+           cmp #E_INT_GAIN_A1D1
            bne +
; inc ATK and inc DEF of Source
Effect_Gain11:
            ldx EfSource
            inc TD_ATK,x
            inc TD_DEF,x
            lda #FX_GAIN_A1D1
            jmp PlayFX

+           cmp #E_INT_ATTACKPLAYER
            bne +
; subtract Source ATK from Player's Life, clamping at 0
Effect_AttackPlayer:
            ldx EfParam                 ; player
            lda PD_LIFE,x
            ldy EfSource                ; table-card
            sec
            sbc TD_ATK,y
            bpl ++
            lda #0
++          sta PD_LIFE,x
            ldx EfSource
            lda #FX_ATTACK_PLAYER
            jmp PlayFX

+           cmp #E_INT_ATTACK
            bne +
; push counter attack (Param attacks Source) and attack (Source attacks Param)
;  note: this causes attacks to be executed "simultaneously" but visually Source goes first
Effect_Attack:
            ldx EfParam                 ; target table-card
            ldy EfSource                ; table-card
            lda #E_INT_ATTACK2
            jsr QueueEffect
            ldx EfSource                ; table-card
            ldy EfParam                 ; target table-card
            lda #E_INT_ATTACK2
            jmp QueueEffect

+           cmp #E_INT_ATTACK2
            bne +
; subtract Source ATK from Param (table-card)'s DEF, clamping at 0
Effect_AttackCard:
            ldx EfParam                 ; target table-card
            lda TD_DEF,x
            ldy EfSource                ; table-card
            sec
            sbc TD_ATK,y
            bpl ++
            lda #0
++          sta TD_DEF,x
            lda TD_ATK,y
            beq .stealrts4
            lda #FX_HURT
            jmp PlayFX

+           cmp #E_INT_DROPCARD
            bne +
; puts card Param at Source onto table and queues its effect
Effect_DropCard:
            ldx EfSource                ; table-card
            ldy EfParam                 ; card#
            lda Cards+CARD_EFFECT,y
            jsr QueueEffect
            lda #FX_DROPCARD
            jmp PlayFX

+           cmp #E_INT_DEADCARD
            bne +
; plays death effect
Effect_DeadCard:
            ldx EfSource                ; table-card
            lda #FX_DEATH
            jmp PlayFX

+           cmp #E_HIT_4
            bne +
; queue hit 4 effect on random enemy
            ldy #4                      ; Y=damage
            lda EfSource                ; Player
            eor #$80
            jsr PickRandomTargetInX     ; X=table-card
            lda #E_INT_HIT
            jmp QueueEffect

+           cmp #E_HIT_3
            bne +
; queue hit 3 effect on random enemy
            ldy #3                      ; Y=damage
            lda EfSource                ; Player
            eor #$80
            jsr PickRandomTargetInX     ; X=table-card
            lda #E_INT_HIT
            jmp QueueEffect

+           cmp #E_HIT_2x2
            bne +
; queue hit 2 effect on 2 random enemies
            ldy #2                      ; Y=damage
            lda EfSource                ; Player
            eor #$80
            jsr PickRandomTargetInX     ; X=table-card
            lda #E_INT_HIT
            jsr QueueEffect
            lda EfSource                ; Player
            eor #$80
            jsr PickRandomTargetInX     ; X=table-card
            lda #E_INT_HIT
            jmp QueueEffect

+           cmp #E_INT_HIT
    	    bne +
; hits table-card Source for Param
            ldx EfSource                ; table-card
            lda TD_CARD,x
            cmp #$FF
            beq .stealrts5              ; fizzle
            lda TD_DEF,x
            sec
            sbc EfParam                 ; damage
            bpl ++
            lda #0
++          sta TD_DEF,x
            lda #FX_HURT
            jmp PlayFX

+           cmp #E_WRAP
            bne +
; queue wrap effect on random selfy
            lda EfSource                ; Player
            jsr PickRandomTargetInX     ; X=table-card
            lda #E_INT_WRAP
            jmp QueueEffect

+           cmp #E_INT_WRAP
            bne +
; adds D3 and Guard to Source
Effect_Wrap:
            ldx EfSource
            lda TD_DEF,x
            clc
            adc #3
            sta TD_DEF,x
            lda TD_STATUS,x
            ora #STATUS_GUARD
            sta TD_STATUS,x
            lda #FX_WRAP
            jmp PlayFX

+           cmp #E_GAIN_D2
            bne +
; queue gain D2 effect on own
            lda EfSource                ; Player
            ora #PD_TABLE
            tax
-           lda TD_CARD,x
            cmp #$FF
            beq ++
            lda #E_INT_GAIN_D2
            jsr QueueEffect
            txa
            clc
            adc #SIZEOF_TD
            tax
            and #$7F                    ; make it work with both Players
            cmp #PlayerData+PD_TABLE+MAX_TABLE*SIZEOF_TD
            bne -
.stealrts5:
++          rts

+           cmp #E_INT_GAIN_D2
            bne +
; adds D2 to Source
Effect_GainD2:
            ldx EfSource                ; table-card
            lda TD_DEF,x
            clc
            adc #2
            sta TD_DEF,x
            lda #FX_GAIN_D2
            jmp PlayFX

+           cmp #E_HIT_ALL_2
            bne .stealrts1
; queue hit 2 effect on all
            ldy #2                      ; Y=damage
            lda EfSource                ; Player
            ora #PD_TABLE
            tax
            jsr .loopall
            lda EfSource                ; Player
            eor #$80
            ora #PD_TABLE
            tax
.loopall:
-           lda TD_CARD,x               ; X=table-card
            cmp #$FF
            beq ++
            lda #E_INT_HIT
            jsr QueueEffect
            txa
            clc
            adc #SIZEOF_TD
            tax
            and #$7F                    ; make it work with both Players
            cmp #PlayerData+PD_TABLE+MAX_TABLE*SIZEOF_TD
            bne -
++          rts


;----------------------------------------------------------------------------
; VISUAL FX PLAYER
;----------------------------------------------------------------------------

; plays effect A on table-card X (clobbers A,X,Y,FxPtr,FxCard,FxScrOff,FxLoop)
PlayFX:
            sta FxPtr
            stx FxCard
            ; set cursor for ColorCard
            lda FXOffsets-(PlayerData+PD_TABLE),x
            sta FxScrOff
            bmi .playfxai
            ; set cursor for DrawTableCard for Player
            ldy #<(SCREEN+15*40+(40-24)/2-8)
            lda #>(SCREEN+15*40+(40-24)/2-8)
            bne +                       ; always
            ; set cursor for DrawTableCard for AI
.playfxai:  ldy #<(SCREEN+4*40+(40-24)/2-8)
            lda #>(SCREEN+4*40+(40-24)/2-8)
+           jsr SetCursor
            lda FXOffsets-(PlayerData+PD_TABLE),x
            and #$7F
            jsr AddToCursor
.playfxloop:
            ldy FxPtr
            lda FxData,y                ; FX #loops
            bne +                       ; 0=end FX
.stealrts1: rts
+           sta FxLoop
            inc FxPtr
-           +WaitVBL($E0)
            ldy FxPtr
            lda FxData,y                ; FX parameter: 0..15=color, <0=tablecard
            bmi +
.playfx3:   ldy FxScrOff
            jsr ColorCard
.playfx2:   dec FxLoop
            bne -
            inc FxPtr
            jmp .playfxloop
+           ldy FxCard
            jsr DrawTableCard
            ldy FxPtr
            lda FxData,y                ; FX parameter: $F0..$FF=color
            cmp #$F0
            bcs .playfx3
            bcc .playfx2                ; always

FXOffsets:
            !for i,0,MAX_TABLE-1 { !byte 8 + i * (TABLE_CARDWIDTH+1),0,0,0 } ; each entry is SIZEOF_TD
!if (*-FXOffsets <> MAX_TABLE*SIZEOF_TD) { !error "FXOffsets table wrong size" }
            !fill $80-SIZEOF_TD*MAX_TABLE,0
            !for i,0,MAX_TABLE-1 { !byte $88 + i * (TABLE_CARDWIDTH+1),0,0,0 } ; each entry is SIZEOF_TD

; scroll text out of view without changing the color (clobbers A,X,Y,Tmp1)
PlayScrollEmpty:
            ldy #S_EMPTY
            beq +                       ; always
; scroll text Y in color A into view (clobbers A,X,Y,Tmp1)
PlayScroll:
            ldx #0
-           sta $D800+11*40,x
            inx
            cpx #40*3
            bne -
+           lda #40
            sta Tmp1
--          +WaitVBL($E0)
            ; scroll
            ldx #0
-           lda SCREEN+11*40+1,x
            sta SCREEN+11*40,x
            inx
            cpx #40*3
            bne -
            lda #CHR_SPACE
            sta SCREEN+11*40+39
            sta SCREEN+13*40+39
            lda ScrollData,y
            sta SCREEN+12*40+39
            iny
            dec Tmp1
            bne --
            rts

; scroll card text based on Card
PlayScrollCard:
            ldy Card
            cpy #$FF
            beq +
            lda Cards+CARD_LTSC,y
            jsr SetSuitColor
            ldx Cards+CARD_NAME,y
            ldy #<ScrollCardNameData
            lda #>ScrollCardNameData
            jsr SetCursorDrawTextX
-           lda #CHR_SPACE
            sta (_CursorPos),y
            iny
            cpy #SIZEOF_SCROLLCARDNAME
            bne -
            ldy #S_CARD_NAME
            lda SuitCol
            jsr PlayScroll
            ldy #<(SCREEN+11*40-40+5)
            lda #>(SCREEN+11*40-40+5)
            jsr SetCursor
            ldx Card
            lda Cards+CARD_GLYPH,x
            ldx #CFG_GLYPH
            ldy #40
            jsr DrawWithOffset
            lda #$FF
            sta Card                    ; only play animation once
+           rts


;----------------------------------------------------------------------------
; UI DRAWING
;----------------------------------------------------------------------------

; draw upper hand with AI card backs (clobbers A,X,Y,Index)
DrawAIHand:
            lda #AIData+PD_HAND
            sta .fixuphandptr
!if DEBUG=1 {
            lda #<DrawPartialCard ; draw visible AI card
} else {
            lda #<DrawPartialCardBack
}
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

; draw upper table (clobbers A,X,Y,TableIdx,Index)
DrawAITable:
            ldy #<(SCREEN+4*40+(40-24)/2)
            lda #>(SCREEN+4*40+(40-24)/2)
            jsr SetCursor
            ldy #AIData+PD_TABLE
            bne .drawtable              ; always
; draw lower table (clobbers A,X,Y,TableIdx,Index)
DrawPlayerTable:
            ldy #<(SCREEN+15*40+(40-24)/2)
            lda #>(SCREEN+15*40+(40-24)/2)
            jsr SetCursor
            ldy #PlayerData+PD_TABLE
.drawtable:
;            ldx #$FF                    ; hide inserting by default
;; draw table in Y at Cursor inserting card A at position X (clobbers A,X,Y,TableIdx,Index)
;DrawTableInserting:
;            stx InsertingPos
;            sta InsertingCard
            ldx #MAX_TABLE
            jsr SetMaxIndexX0
-           sty TableIdx                ; starts at $1C or $9C
;            cpx InsertingPos
;            beq .drawinsert
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
; .drawinsert:
;             ldx InsertingCard
;             jsr DrawCard
;             lda #TABLE_CARDWIDTH+1
;             jsr AddToCursor
;             ldy TableIdx
;             bne --                      ; always


;----------------------------------------------------------------------------
; CARD DRAWING
;----------------------------------------------------------------------------

; draws table-card Y at Cursor (clobbers A,X,Y)
; - draws status border, no decoration and highlighted attack/defense
DrawTableCard:
            ldx TD_CARD,y
            cpx #$FF
            bne +
            ldx #CFG_CLEARFRAME         ; clear card
            jmp Draw
+           jsr DecorateBottomFrame     ; set (default) atk/def values
            ; disable recoloring when tapped
            lda #$85                    ; STA
            sta .fixupcolorwrite        ; enable color write
            lda TD_STATUS,y
            and #STATUS_TAPPED
            beq +
            lda #COL_DISABLED
            sta CharCol
            sta SuitCol
            lda #$24                    ; BIT
            sta .fixupcolorwrite        ; disable color write
+           lda TD_STATUS,y
            and #STATUS_GUARD
            bne .guard
            ; regular card
            tya                         ; backup card in Y
            pha
            lda Cards+CARD_GLYPH,x
            ldx #CFG_GLYPH
            ldy #40
            jsr DrawWithOffset
            lda #FRAME_TABLE_CARD
            ldx #CFG_FRAME
            jsr Draw
            pla
            tax                         ; restore card to X
            bne .setatkdef              ; always
.guard:     ; guard card
            tya                         ; backup card in Y
            pha
            lda Cards+CARD_GLYPH,x
            ldx #CFG_GLYPH
            ldy #40
            jsr DrawWithOffset
            lda #FRAME_GUARD
            ldx #CFG_FRAME
            jsr Draw
            pla
            tax                         ; restore card to X
            ; update attack value with actual
.setatkdef: ldy #5*40+1                 ; ATK position
            jsr .updateatkdef
            ; update defense value with actual
            inx ; CAREFUL: THIS ASSUMES DEFENSE COMES DIRECTLY AFTER ATTACK IN MEMORY
            ldy #5*40+4                 ; DEF position
; common code to read screen to determine high/low/same, X=table index
.updateatkdef:
            lda (_CursorPos),y
            and #$0F
            cmp TD_ATK,x
            beq +                       ; same -> done
            bcc .higher
            lda #COL_LOWER
            +SKIP2
.higher:    lda #COL_HIGHER
            .fixupcolorwrite=*
            sta CharCol                 ; SELF-MODIFIED $85=STA $24=BIT
            lda TD_ATK,x
            clc
            adc #$30                    ; regular digits
            cmp #$3A                    ; 10 or higher?
            bcc ++
            lda #'?'                    ; yes, draw '?' symbol TODO instead draw second digit
++          sta (_CursorPos),y
            lda CharCol
            sta (_ColorPos),y
+           rts

; decorates top and bottom of frame according to card in X (clobbers A)
DecorateFrame:
            ; setup type/cost
            lda Cards+CARD_LTSC,x
            and #%01000000              ; LTSSCCCC T=Type(0=Spell/1=Monster)
            asl
            asl                         ; C=T (0=spell, 1=monster)
            adc #$D7                    ; T=0->D7(ball) T=1->D8(clover)
            sta frame_TYPE
            lda Cards+CARD_LTSC,x
            and #%00001111              ; LTSSCCCC
            ora #$B0                    ; 0..9 reversed
            sta frame_COST
            ; fall through

; decorates bottom of frame according to card in X (clobbers A)
DecorateBottomFrame:
            ; setup ATK/DEF
            lda Cards+CARD_ATDF,x
            and #$0F
            ora #$B0
            sta frame_DEF  ; Regular frame
            sta frame_DEF2 ; Table card frame
            sta frame_DEF3 ; Guard frame
            lda Cards+CARD_ATDF,x
            lsr
            lsr
            lsr
            lsr
            ora #$B0
            sta frame_ATK
            sta frame_ATK2
            sta frame_ATK3
            ; fall through

; sets CharCol and SuitCol according to card in X (clobbers A)
SetColors:
            lda Cards+CARD_LTSC,x
            bmi + ; legend
            lda #COL_PLAIN
            +SKIP2
+           lda #COL_LEGEND
            sta CharCol
            lda Cards+CARD_LTSC,x
SetSuitColor:
            lsr
            lsr
            lsr
            lsr
            and #$03
            sta SuitCol
            rts

; draws decorated card in X at Cursor (clobbers A,X,Y)
; - draws legend border, full decoration and default attack/defense
DrawCard:
            jsr DecorateFrame
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

            !align $0F,0,0 ; force DrawPartialCard* in same page

; draws bottom 2-lines of card background at Cursor (clobbers A,X,Y)
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

; colors card with color A at offset Y <$80 is Player, >=$80 is AI (clobbers X,Y)
ColorCard:
            ldx #5
            cpy #$80
            bcc .ColorPlayerCard
-           sta $D800+(4+0)*40-$80,y
            sta $D800+(4+1)*40-$80,y
            sta $D800+(4+2)*40-$80,y
            sta $D800+(4+3)*40-$80,y
            sta $D800+(4+4)*40-$80,y
            sta $D800+(4+5)*40-$80,y
            iny
            dex
            bne -
            rts

; colors player card with color A at offset Y (clobbers X,Y)
.ColorPlayerCard:
-           sta $D800+(15+0)*40,y
            sta $D800+(15+1)*40,y
            sta $D800+(15+2)*40,y
            sta $D800+(15+3)*40,y
            sta $D800+(15+4)*40,y
            sta $D800+(15+5)*40,y
            iny
            dex
            bne -
            rts

NEWSTYLE=0
!if NEWSTYLE=1 {
; draws glyph of AI card in X at offset Y in SuitCol (clobbers A,X,Y)
DrawAIGlyph:
            lda Cards+CARD_GLYPH,x      ; glyph offset
            tax
            lda GlyphData,x
            sta SCREEN+(4+0)*40,y
            lda GlyphData+3,x
            sta SCREEN+(4+1)*40,y
            lda GlyphData+6,x
            sta SCREEN+(4+2)*40,y
            lda SuitCol
            sta $D800+(4+0)*40,y
            sta $D800+(4+1)*40,y
            sta $D800+(4+2)*40,y
            inx
            iny
            rts

DrawPlayerGlyph:
            lda Cards+CARD_GLYPH,x      ; glyph offset
            tax
            lda GlyphData,x
            sta SCREEN+(15+0)*40,y
            lda GlyphData+3,x
            sta SCREEN+(15+1)*40,y
            lda GlyphData+6,x
            sta SCREEN+(15+2)*40,y
            lda SuitCol
            sta $D800+(15+0)*40,y
            sta $D800+(15+1)*40,y
            sta $D800+(15+2)*40,y
            inx
            iny
            rts
}


;----------------------------------------------------------------------------
; CARDS
;----------------------------------------------------------------------------
CARD_LTSC=0         ; 1 byte LTSSCCCC : L=Legendary T=Type(0=Spell/1=Monster) SS=Suit(0,1,2,3) CCCC=Cost(0..15)
CARD_ATDF=1         ; 1 byte AAAADDDD : AAAA=Attack DDDD=Defense
CARD_NAME=2         ; 1 byte Name TextPtr
CARD_EFFECT=3       ; 1 byte Effect TextPtr (also used to perform effect)
CARD_GLYPH=4        ; 1 byte GlyphPtr
SIZEOF_CARD=5       ; 256/5 => 50 cards max (0 and $FF are used for other purposes)

Cards:
    !byte 0 ; card# (offsets) should not be 0
    C_GOBLIN_LEADER=*-Cards
    !byte $C3, $34, N_GOBLIN_LEADER, E_ALL_GAIN_A1D1, G_LEGND_GOBLIN
    !byte $41, $11, N_WANNABE,       E_READY,      G_WANNABE
    !byte $44, $35, N_SHIELDMASTA,   E_GUARD,      G_3
    !byte $42, $32, N_GRUNT,         T_NONE,       G_4
    !byte $45, $46, N_BRUISER,       T_NONE,       G_5
    !byte $02, $00, N_GOBLIN_BOMB,   E_HIT_3,      G_BOMB
    !byte $03, $00, N_GOBLIN_ROCKET, E_HIT_2x2,    G_ROCKET
    !byte $05, $00, N_GOBLIN_FIRE,   E_HIT_ALL_2,  G_FIRE
    C_POLY_LEADER=*-Cards
    !byte $D3, $07, N_POLY_LEADER,   E_GUARD,      G_LEGND_POLY
    !byte $51, $12, N_WANNABE,       T_NONE,       G_WANNABE
    !byte $52, $22, N_PLASTIC_CUP,   E_GUARD,      G_10
    !byte $52, $13, N_LEGO,          E_READY,      G_14
    !byte $54, $45, N_PVC,           T_NONE,       G_13
    C_PLASTIC_WRAP=*-Cards
    !byte $12, $00, N_PLASTIC_WRAP,  E_WRAP,       G_16
    !byte $15, $00, N_PUR_FOAM,      E_GAIN_D2,    G_11
    !byte $13, $00, N_PLASTIC_KNIFE, E_HIT_4,      G_12
    C_CANDY_LEADER=*-Cards
    !byte $E2, $02, N_CANDY_LEADER,  T_NONE,       G_LEGND_CANDY
    !byte $61, $11, N_WANNABE,       T_NONE,       G_WANNABE
    !byte $62, $12, N_WANNABE,       T_NONE,       G_WANNABE
    !byte $63, $13, N_WANNABE,       T_NONE,       G_WANNABE
    !byte $64, $14, N_WANNABE,       T_NONE,       G_WANNABE
    !byte $65, $15, N_WANNABE,       T_NONE,       G_WANNABE
    !byte $66, $16, N_WANNABE,       T_NONE,       G_WANNABE
    !byte $27, $17, N_WANNABE,       T_NONE,       G_WANNABE
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
    ; G_2=*-GlyphData
    ; !byte 73,104,85
    ; !byte 118,215,215
    ; !byte 40,58,87
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
    G_BOMB=*-GlyphData
    !byte 32,85,73
    !byte 233,160,93
    !byte 95,105,32
    G_FIRE=*-GlyphData
    !byte 32,92,92
    !byte 92,102,163
    !byte 163,163,102
    G_ROCKET=*-GlyphData
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
    !byte 46,46,46
    !byte 160,160,160
    !byte 32,32,32
    ; G_15=*-GlyphData
    ; !byte 85,64,73
    ; !byte 67,67,67
    ; !byte 74,64,75
    G_16=*-GlyphData
    !byte 215,215,215
    !byte 215,215,215
    !byte 215,215,215
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
    E_ALL_GAIN_A1D1=*-TextData ; All other cards of the same suit gain A1/D1
    E_INT_GAIN_A1D1=*-TextData+1
    !byte M_ALL,M_SUIT,M_GAIN,M_A1D1,0
    E_READY=*-TextData ; Card has no summoning sickness
    !byte M_READY,0
    E_GUARD=*-TextData ; Card blocks
    !byte M_GUARD,0
    E_HIT_3=*-TextData ; Hit (enemy) for 3
    !byte M_HIT,M_FOR,M_A3,0
    E_HIT_2x2=*-TextData ; Hit 2x (enemy) for 2
    !byte M_HIT,M_2X,M_FOR,M_A2,0
    E_HIT_ALL_2=*-TextData ; Hit all for 2
    !byte M_HIT,M_ALL,M_FOR,M_A2,0
    E_HIT_4=*-TextData ; Hit (enemy) for 4
    !byte M_HIT,M_FOR,M_A4,0
    E_WRAP=*-TextData ; Add D3 and guard
    E_INT_WRAP=*-TextData+1
    !byte M_GIVE,M_D3,M_AND,M_GUARD,0
    E_GAIN_D2=*-TextData ; Add D2 to own
    E_INT_GAIN_D2=*-TextData+1
    !byte M_ALL,M_YOUR,M_GAIN,M_D2,0
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
    N_SHIELDMASTA=*-TextData
    !byte M_GOBLIN,M_SHIELDMASTA,0
    N_GRUNT=*-TextData
    !byte M_GOBLIN,M_GRUNT,0
    N_BRUISER=*-TextData
    !byte M_GOBLIN,M_BRUISER,0
    N_GOBLIN_BOMB=*-TextData
    !byte M_GOBLIN,M_BOMB,0
    N_GOBLIN_FIRE=*-TextData
    !byte M_GOBLIN,M_FIRE,0
    N_GOBLIN_ROCKET=*-TextData
    !byte M_GOBLIN,M_ROCKET,0
    N_PLASTIC_CUP=*-TextData
    !byte M_PLASTIC,M_CUP,0
    N_LEGO=*-TextData
    !byte M_LEGO,0
    N_PVC=*-TextData
    !byte M_PVC,0
    N_PLASTIC_WRAP=*-TextData
    !byte M_PLASTIC,M_WRAP,0
    N_PUR_FOAM=*-TextData
    !byte M_PUR,M_FOAM,0
    N_PLASTIC_KNIFE=*-TextData
    !byte M_PLASTIC,M_KNIFE,0
!if *-TextData >= $FF { !error "Out of TextData memory" }
; Additional effects - make sure they don't map to any string used as CARD_EFFECT (i.e. 2 or 15)
E_INT_ATTACKPLAYER=3
E_INT_ATTACK=4
E_INT_ATTACK2=5
E_INT_DROPCARD=6
E_INT_DEADCARD=7
E_INT_HIT=8

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
    M_AND         =*-MacroData+1 : !scr "an",'d'+$80
    M_GAIN        =*-MacroData+1 : !scr "gai",'n'+$80
    M_A2          =*-MacroData+1 : !scr CHR_ATK,'2'+$80
    M_A3          =*-MacroData+1 : !scr CHR_ATK,'3'+$80
    M_A4          =*-MacroData+1 : !scr CHR_ATK,'4'+$80
    M_A1D1        =*-MacroData+1 : !scr CHR_ATK,'1',CHR_DEF,'1'+$80
    M_D2          =*-MacroData+1 : !scr CHR_DEF,'2'+$80
    M_D3          =*-MacroData+1 : !scr CHR_DEF,'3'+$80
    M_READY       =*-MacroData+1 : !scr "read",'y'+$80
    M_GUARD       =*-MacroData+1 : !scr "guar",'d'+$80
    M_GIVE        =*-MacroData+1 : !scr "giv",'e'+$80
    M_HIT         =*-MacroData+1 : !scr "hi",'t'+$80
    M_2X          =*-MacroData+1 : !scr "2",'x'+$80
    M_FOR         =*-MacroData+1 : !scr "fo",'r'+$80
    M_SHIELDMASTA =*-MacroData+1 : !scr "shieldmast",'a'+$80
    M_GRUNT       =*-MacroData+1 : !scr "grun",'t'+$80
    M_BRUISER     =*-MacroData+1 : !scr "bruise",'r'+$80
    M_BOMB        =*-MacroData+1 : !scr "bom",'b'+$80
    M_FIRE        =*-MacroData+1 : !scr "fir",'e'+$80
    M_ROCKET      =*-MacroData+1 : !scr "rocke",'t'+$80
    M_PLASTIC     =*-MacroData+1 : !scr "plasti",'c'+$80
    M_CUP         =*-MacroData+1 : !scr "cu",'p'+$80
    M_LEGO        =*-MacroData+1 : !scr "leg",'o'+$80
    M_PVC         =*-MacroData+1 : !scr "pv",'c'+$80
    M_WRAP        =*-MacroData+1 : !scr "wra",'p'+$80
    M_PUR         =*-MacroData+1 : !scr "pu",'r'+$80
    M_KNIFE       =*-MacroData+1 : !scr "knif",'e'+$80
!if *-MacroData >= $FF { !error "Out of MacroData memory" }
    !byte *-MacroData ; DEBUG

AINames:
    !scr "bd","jt","mg","rh"


;----------------------------------------------------------------------------
; SCROLL TEXTS
;----------------------------------------------------------------------------

ScrollData:
    S_EMPTY=*-ScrollData
    !fill 40-((40-8)/2)," "
    S_WIN=*-ScrollData
    !fill (40-8)/2," "
    !scr " you win! " ;10
    !fill (40-8)/2," "
    S_LOSE=*-ScrollData
    !fill (40-12)/2," "
    !scr " game over! " ;12
    !fill (40-12)/2," "
    S_YOURTURN=*-ScrollData
    !fill (40-14)/2," "
    !scr " your turn... " ;14
    !fill (40-14)/2," "
    S_OPPONENTSTURN=*-ScrollData
    !fill (40-20)/2," "
    !scr " opponent's turn... " ;20
    S_CARD_NAME=*-ScrollData
    !fill (40-20)/2," "
ScrollCardNameData:
    !fill 40-((40-20)/2)," "
SIZEOF_SCROLLCARDNAME=*-ScrollCardNameData


;----------------------------------------------------------------------------
; FRAMES
;----------------------------------------------------------------------------

; DrawF_Frame source data, use $60/96 to skip over a char
FrameData:
    FRAME_CARD=*-FrameData              ; Full 5x6 decorated card frame
    FRAME2_CARD=*-FrameData             ; 5x2 top of decorated card frame
frame_TYPE: !byte 79
frame_COST: !byte 119
    !byte 119,119,80
    !byte 116,96,96,96,106
    !byte 116,96,96,96,106
    !byte 116,96,96,96,106
    !byte 76,111,111,111,122
    !byte CHR_ATK|$80
frame_ATK: !byte 160
    !byte 160,CHR_DEF|$80
frame_DEF: !byte 160
    FRAME_TABLE_CARD=*-FrameData        ; Full 5x6 table card frame
    !byte 79,119,119,119,80
    !byte 116,96,96,96,106
    !byte 116,96,96,96,106
    !byte 116,96,96,96,106
    !byte 76,111,111,111,122
    !byte CHR_ATK|$80
frame_ATK2: !byte 160
    !byte 160,CHR_DEF|$80
frame_DEF2: !byte 160
    FRAME_CARDBACK=*-FrameData          ; Full 5x6 card back
    !byte 236,192,192,192,251
    !byte 194,102,102,102,194
    !byte 194,102,102,102,194
    !byte 194,102,102,102,194
    FRAME2_CARDBACK=*-FrameData         ; 5x2 bottom of card back
    !byte 194,102,102,102,194
    !byte 252,192,192,192,254
    FRAME_GUARD=*-FrameData             ; Full 5x6 Guard card frame
    !byte 135,149,129,146,132
    !byte 116,96,96,96,106
    !byte 116,96,96,96,106
    !byte 116,96,96,96,106
    !byte 76,111,111,111,122
    !byte CHR_ATK|$80
frame_ATK3: !byte 160
    !byte 160,CHR_DEF|$80
frame_DEF3: !byte 160
    ; FRAME_SHIELD=*-FrameData            ; 5x5 Shield card frame
    ; !byte 255,119,119,119,127
    ; !byte 116,96,96,96,106
    ; !byte 116,96,96,96,106
    ; !byte 116,96,96,96,106
    ; !byte 127,111,111,111,255


;----------------------------------------------------------------------------
; COLOR FX
;----------------------------------------------------------------------------

; Each FX is a list of color changes for a single card TODO accompanied by a sound FX
FxData:
    ; duration,color
    ;  duration: #frames, list ends with duration=0
    ;  color: $00..$0F=color, $80=draw table card, $F0..$FF=draw table card in color
    FX_UNTAP=*-FxData
    FX_GUARD=*-FxData
    !byte 20,$F0+WHITE
    !byte 0
    FX_GAIN_A1D1=*-FxData
    FX_GAIN_D2=*-FxData
    FX_WRAP=*-FxData
    !byte 20,$F0+GREEN
    !byte 20,WHITE
    !byte 10,GREEN
    !byte 20,WHITE
    !byte 0
    FX_DROPCARD=*-FxData
    !byte 5,$80
    !byte 5,COL_SCREEN
    !byte 5,$80
    !byte 5,COL_SCREEN
    !byte 5,$80
    !byte 5,COL_SCREEN
    !byte 5,$80
    !byte 0
    FX_HURT=*-FxData
    !byte 5,$F0+LIGHT_RED
    !byte 5,PURPLE
    !byte 10,RED
    !byte 0
    FX_DEATH=*-FxData
    !byte 50,BLACK
    !byte 0
    FX_ATTACK_PLAYER=*-FxData
    !byte 20,$80
    !byte 0


;----------------------------------------------------------------------------
; RUN TIME DATA
;----------------------------------------------------------------------------

; Effect Queue (SoA)
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
