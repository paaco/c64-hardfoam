; HARD FOAM - a 2K card game
; Developed for the https://itch.io/jam/the-c64-cassette-50-charity-competition

; Only WRITES memory < $1000 and uses Dxxx IO, calling/reading KERNAL/BASIC is OK

; Note that it is only required to load below $1000, not specifically $0801,
; so we could even load at $0400 (not lower to keep Tape loading compatibility)
; However, loading it there (anything below $0801) will kill RUN, only allow direct SYS
; Exomizer also uses $0334-$03D0 as decrunching buffer; decrunching there will hang

; Without packer it's possible to load and run $0120-$1000 giving 3808 bytes:
; Holes at $1ED-$01F9, $028D,$028E, $02A1, $0314-$032A (vectors) and $0400-$07E8 (screen)
; Keeping 5 screen rows for code adds 200 bytes

; TODO: turn: play cards from your hand
; TODO: turn: attack with table cards (pick your own order, attack not required)
; TODO: end turn
; TODO: AI turn: take card; if necessary discard the (first)) most expensive one
; TODO: AI turn: while possible, play random cards from hand
; TODO: AI turn: with each table card, attack random table card
; TODO: AI turn: if there are no opponent table cards left, attack player
; TODO: AI end turn

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
COL_DISABLED=PURPLE
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
!addr Tmp3=$0C
!addr TmpText=$0D
;!addr Suit=$0E
!addr Joystick=$0F
; player data (consecutive)
!addr PlayerData=$10
    PD_LIFE=0       ; 0 .. 10
    PD_ENERGY=1     ; $30 .. $39
    ; fixed '/' character in between
    PD_MAXENERGY=3  ; $30 .. $39
    PD_REMAIN=4     ; DECKSIZE-1 .. 0
    PD_HAND=5       ; 7 bytes (card#),$FF=no card
    SIZEOF_HAND=6   ; max 6 cards
    PD_TABLE=12     ; 20 bytes 5*4 bytes (card#,atd,def,status)
      TD_CARD=0     ; card# $FF=no card
      TD_ATK=1      ; attack
      TD_DEF=2      ; defense
      TD_STATUS=3   ; status: 0=normal, 1=tapped, >=$80 selected
      SIZEOF_TD=4
    PD_DECK=32      ; 32 bytes (card#)
SIZEOF_PD=64
!addr AIData=$10+SIZEOF_PD
!addr Index=$90     ; loop/selection index
!addr MaxIndex=$91  ; <MaxIndex
!addr SelectorIndex=$92 ; Index used for DrawCardSelect/ClearCardSelect
;!addr CardIdx=$92
;!addr TableIdx=$93
; Draws rectangle 5x5 (upto 8x6) via DrawF function (clobbers A,Y)
!addr _Draw=$E0     ; $E0-$F8 is block drawing routine

;############################################################################
*=$0120     ; DATA (0120-01ED = 282 bytes)

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
            jmp _Draw+2

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

; clears the two lower lines (clobbers A,Y)
ClearLowerLines: ; 14 bytes
            lda #CHR_SPACE
            ldy #38
-           sta $0400+21*40,y
            sta $0400+22*40,y
            dey
            bpl -
            rts

; clears two lines upper lines (clobbers A,Y)
ClearUpperLines: ; 14 bytes
            lda #CHR_SPACE
            ldy #38
-           sta $0400+2*40,y
            sta $0400+3*40,y
            dey
            bpl -
            rts

            !fill 24,$EE ; remaining

;############################################################################
*=$01ED     ; DATA 13 bytes including return address (TRASHED WHILE LOADING)
Overwrite01ED:

; initial PlayerData structure (rest is filled with $FF)
InitData:
    !scr 10, "0/0", 0
SIZEOF_INITDATA = *-InitData

SuitTextData:
    !byte M_GOBLIN,M_POLYSTYRENE,M_CANDY,M_SOAP
;SuitLeaders:
;    !byte C_POLY_LEADER,C_CANDY_LEADER;,C_SOAP_LEADER,C_GOBLIN_LEADER

*=$01F8     ; Override return value on stack with own start address
            !word INIT-1

;############################################################################
*=$01FA     ; DATA (01FA-028C = 147 bytes)

; wipes the top and bottom of the screen completely (clobbers A,Y)
ClearAll: ; 20 bytes
            lda #CHR_SPACE
            ldy #5*40
-           sta $0400-1,y
            sta $0400-1+5*40,y
            sta $0400-1+15*40,y
            sta $0400-1+20*40,y
            dey
            bne -
            rts

; draws energy, deck counters and health bards (clobbers A,X,Y,Tmp1)
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

            ; draw both health bars
            lda #COL_HEALTH_ON
            sta CharCol
            lda #10
            sec
            sbc AIData+PD_LIFE
            tax
            ldy #<($0400+39)
            lda #>($0400+39)
            jsr .healthbar
            lda #COL_HEALTH_OFF
            sta CharCol
            ldx PlayerData+PD_LIFE
            ldy #<($0400+15*40+39)
            lda #>($0400+15*40+39)
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

;############################################################################
*=$0277     ; 0277-0280 KEYBOARD BUFFER. SOME VERSIONS OF VICE TRASH 5 bytes HERE WITH: RUN:^M
            !fill 5,0

            !fill 17,$EE ; remaining

;############################################################################
*=$028D     ; 028D-028E (first 2 bytes) TRASHED DURING LOADING

; Converts .A to 3 ASCII/PETSCII digits: .Y = hundreds, .X = tens, .A = ones
AtoASCII2: ; 20 bytes (for 2 digits)
            ; ldy #$2f
            ldx #$3a                    ; $a2 $3a SELF-MODIFIED TRASHED DURING LOADING
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

;            !fill 20,$EE ; remaining

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

        !fill 4,$EE ; remaining

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
            jmp INIT

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
            sta $0400+21*40+(40-28)/2,x
            lda #COL_LEGEND
            sta $D800+21*40+(40-28)/2,x
            dex
            bpl -
            rts

Alexander:
            !scrxor $AA, "a game by alexander paalvast" ; 28 bytes

*=$0400+3*40+(40-16)/2 ; $0484 above logo so it is still alive
            !scr "twain pain games" ; 16 bytes

*=$0400+5*40 ; 04C8(1224) - 5 lines logo will overwrite from here
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

            ; restore 01ED-01F9
            ldx #SIZEOF_Overwrite01EDCopy-1
-           lda Overwrite01EDCopy,x
            sta Overwrite01ED,x
            dex
            bpl -

            ; restore 028D-028E (AtoASCII2)
            lda #$a2                    ; LDX
            sta $028D
            lda #$3a                    ; #$3a
            sta $028E

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

Overwrite01EDCopy:
;InitData:
    !scr 10, "0/0", 28
;SuitTextData:
    !byte M_GOBLIN,M_POLYSTYRENE,M_CANDY,M_SOAP
;SuitLeaders:
;    !byte C_POLY_LEADER,C_CANDY_LEADER,C_SOAP_LEADER,C_GOBLIN_LEADER
SIZEOF_Overwrite01EDCopy=*-Overwrite01EDCopy

; Draws rectangle 5x5 (upto 8x6) via DrawF function (clobbers A,Y)
INITDRAW: ; 24 bytes incl configure (PIC)
!pseudopc _Draw {
            ldy #0
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

;############################################################################
*=$0590     ; DATA (200 bytes, MIDDLE 5 SCREEN LINES ARE HIDDEN)

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

;############################################################################
*=$0658     ; SCREEN (WILL BE WIPED)

            ; 5 lines logo will overwrite here

*=$0658+5*40 ; in lowest 5*40=200 bytes (will not be overwritten with logo)
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
-           ldy $0400,x
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
            jmp LogoDone

.drawlogo:
            ldx #0
--          ldy #10 ; 10 pixels = 8 from the byte and 2 empty
            .logosrc = *+1
-           asl logo,x
            bcc +
            lda #81                     ; ball
            .logoptr = *+1
            sta $0400+1+5*40
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

            !fill 17,$EE ; remaining WIPED

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
            ldy #<($0400+5*40+9)
            lda #>($0400+5*40+9)
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
            ldy #<($0400+9*40+(40-17)/2)
            lda #>($0400+9*40+(40-17)/2)
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
            ldy #<($0400+15*40+(40-24)/2)
            lda #>($0400+15*40+(40-24)/2)
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
            ldy #<($0400+21*40+(40-24)/2)
            lda #>($0400+21*40+(40-24)/2)
            ldx #T_SUIT_DECK
            jsr SetCursorDrawTextX

            ldy #<($0400+15*40+(40-24)/2)
            lda #>($0400+15*40+(40-24)/2)
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
-           ldx #PlayerData
            jsr PullDeckCard
            ldx #AIData
            jsr PullDeckCard
            dey
            bne -

            ; redraw screen with opponent at top
            jsr ClearAll
            lda #COL_PLAIN
            sta SuitCol
            ldx #T_OPPONENT_NAME
            ldy #<($0400+0*40+4)
            lda #>($0400+0*40+4)
            jsr SetCursorDrawTextX
            ldy #<($0400+4*40)
            lda #>($0400+4*40)
            jsr SetCursorDrawCardBack
            jsr DrawAIHand

            ; restore energy for player
            ldx #PlayerData
            jsr Energize

            jsr DrawCounters

            ; TODO: start of turn: select first possible hand card
            ; TODO: if there is none, select first table card
            ; TODO: if there is none, select END symbol

            ; select from hand
--          lda #0                      ; 0..SIZEOF_HAND-1=card, SIZEOF_HAND=END
            sta SelectorIndex

            ; draw selected card for player (or card back)
.redraw:    ldy #<($0400+15*40)
            lda #>($0400+15*40)
            jsr SetCursor
            jsr ClearLowerLines         ; clears text area
            ldy SelectorIndex
            ldx PlayerData+PD_HAND,y
            cpx #$FF
            bne +
            jsr DrawCardBack
            jmp ++
+           txa
            pha
            jsr DrawCard
            pla
            tax
            ldy #<($0400+21*40)
            lda #>($0400+21*40)
            jsr SetCursorDrawCardText

++          jsr DrawPlayerHand
            lda #COL_SELECTED
            sta SuitCol
            ldy #<($0400+23*40+35)
            lda #>($0400+23*40)
            ldx #T_END
            jsr SetCursorDrawTextX

            ; draw selection symbol
            ldy #<($0400+23*40)
            lda #>($0400+23*40)
            jsr SetCursor
            ldx SelectorIndex
            cpx #SIZEOF_HAND
            bne +
            lda #CHR_ENDTURN
            bne ++                      ; always
+           lda PlayerData+PD_HAND,x
            cmp #$FF                    ; is there a card?
            beq .no_energy              ; no card -> handle as if no energy
            tay
            lda Cards+CARD_LTSC,y
            and #%00001111              ; LTSSCCCC
            ora #$30                    ; same as energy on screen
            cmp $0400+24*40             ; energy on screen
            beq .can_afford
            bcc .can_afford
.no_energy: lda #CHR_NO_PLAY
            bne ++                      ; always
.can_afford:lda #CHR_PLAY
++          ldy HandSelectorOffsets,x
            jsr DrawF_ASuitCol

            jsr DebounceJoystick
-           jsr ReadJoystick            ; 111FRLDU
            beq -
            cmp #%11110111              ; RIGHT
            bne +
            inc SelectorIndex
+           cmp #%11111011              ; LEFT
            bne +
            dec SelectorIndex

+           jmp .redraw

HandSelectorOffsets:
    !for i,0,6 { !byte 10+HAND_CARDWIDTH*i }


;----------------------------------------------------------------------------
; GAME FUNCTIONS
;----------------------------------------------------------------------------

; pull a card in hand for Player in X (clobbers A,X) returns pulled card# >0 in A (or 0)
; if there's no room in hand, the card is lost
PullDeckCard:
            lda PD_REMAIN,x             ; A=remaining, X=offset to PD ($10/$50)
            beq ++                      ; no cards left!
            dec PD_REMAIN,x
            clc
            adc #PD_DECK-1              ; A=PD_DECK + remaining-1 (offset to card)
            sta .fixupdeckptr
            .fixupdeckptr=*+1
            lda PD_DECK,x               ; A=card# SELF-MODIFIED
            pha                         ; store card#
            ; look for room in hand
-           lda PD_HAND,x
            cmp #$FF
            bne +
            pla                         ; found room
            sta PD_HAND,x
++          rts
+           inx
            txa
            and #%00000111
            cmp #SIZEOF_HAND
            bne -
            pla                         ; no room
.stealrts3: rts

; increase max energy and replenish energy of Player in X (clobbers Y)
Energize: ; 12 bytes
            ldy PD_MAXENERGY,x
            cpy #$39
            beq +
            iny
            sty PD_MAXENERGY,x
+           sty PD_ENERGY,x
            rts

; decrease energy (capping at 0) of Player in X (clobbers A)
DecreaseEnergy:
            clc
            sbc PD_ENERGY,x             ; A=A-(PD_ENERGY-1)
            eor #$FF                    ; $100-A
            cmp #$30
            bcs +
            lda #$30                    ; clamp at $30 ('0')
+           sta PD_ENERGY,x
            rts


;----------------------------------------------------------------------------
; UI DRAWING
;----------------------------------------------------------------------------

; draw upper hand with AI card backs (clobbers A,X,Y,Index)
DrawAIHand:
            lda #AIData+PD_HAND
            sta .fixuphandptr
            lda #<DrawPartialCardBack
            sta .fixupdrawcard
            ldy #<($0400+0*40+8)
            lda #>($0400+0*40+8)
            bne .drawhand               ; always

; draws lower hand with visible Player cards (clobbers A,X,Y,Index)
DrawPlayerHand:
            lda #PlayerData+PD_HAND
            sta .fixuphandptr
            lda #<DrawPartialCard
            sta .fixupdrawcard
            ldy #<($0400+23*40+8)
            lda #>($0400+23*40+8)
.drawhand:
            jsr SetCursor
            ldx #SIZEOF_HAND
            jsr SetMaxIndexX0
            .fixuphandptr=*+1
-           lda AIData+PD_HAND,x
            cmp #$FF
            beq +
            tax
            .fixupdrawcard=*+1
            jsr DrawPartialCardBack
            lda #HAND_CARDWIDTH
            jsr AddToCursor
+           jsr IncIndex
            bne -
            ; fill remainder after cursor with spaces
-           lda _CursorPos
            cmp #38                     ; end for AI hand
            beq .stealrts3
            cmp #<(23*40+38)            ; end for Player hand
            beq .stealrts3
            inc _CursorPos
            lda #CHR_SPACE
            ldy #0
            sta (_CursorPos),y
            ldy #40
            sta (_CursorPos),y
            bne -                       ; always


;----------------------------------------------------------------------------
; CARD DRAWING
;----------------------------------------------------------------------------

; draws card on table in Y at Cursor (clobbers A,X,Y)
; - draws status border, no decoration and highlighted attack/defense
DrawTableCard:
            ldx TD_CARD,y
            cmp #$FF
            bne +
            ldx #CFG_CLEARFRAME         ; clear card
            jmp Draw
+           jsr DecorateFrame
            ; hide type/cost
            lda #79                     ; left corner
            sta frame_TYPE
            lda #119                    ; top line
            sta frame_COST
            ; override colors if tapped
            lda #$85                    ; STA
            sta .fixupcolorwrite        ; enable color write
            lda TD_STATUS,y             ; 0=normal, 1=tapped
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
            lda $0400+24*40             ; energy on screen
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
SIZEOF_CARD=5

Cards:
    !byte 0 ; card# (offsets) should not be 0
    C_GOBLIN_LEADER=*-Cards
    !byte $C3, $32, N_GOBLIN_LEADER, E_ALL_GAIN11, G_LEGND_GOBLIN
    !byte $01, $12, N_WANNABE,       T_NONE,       G_WANNABE
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

; default decks (8 bytes each, starting with legendary)
Decks:
    !byte C_GOBLIN_LEADER,C_GOBLIN_LEADER+5,C_GOBLIN_LEADER+10,C_GOBLIN_LEADER+15,C_GOBLIN_LEADER+20,C_GOBLIN_LEADER+25,C_GOBLIN_LEADER+30,C_GOBLIN_LEADER+35
    !byte C_POLY_LEADER,C_POLY_LEADER+5,C_POLY_LEADER+10,C_POLY_LEADER+15,C_POLY_LEADER+20,C_POLY_LEADER+25,C_POLY_LEADER+30,C_POLY_LEADER+35
    !byte C_CANDY_LEADER,C_CANDY_LEADER+5,C_CANDY_LEADER+10,C_CANDY_LEADER+15,C_CANDY_LEADER+20,C_CANDY_LEADER+25,C_CANDY_LEADER+30,C_CANDY_LEADER+35
    !byte C_SOAP_LEADER,C_SOAP_LEADER+5,C_SOAP_LEADER+10,C_SOAP_LEADER+15,C_SOAP_LEADER+20,C_SOAP_LEADER+25,C_SOAP_LEADER+30,C_SOAP_LEADER+35


;----------------------------------------------------------------------------
; GLYPHS
;----------------------------------------------------------------------------

; glyphs, 3 bytes per glyph = max 252/9 = 28 glyphs
GlyphData:
    G_LEGND_GOBLIN=*-GlyphData
    !byte 73,104,85
    !byte 215,215,117
    !byte 81,73,41
    G_LEGND_POLY=*-GlyphData
    !byte 206,160,205
    !byte 160,182,160
    !byte 192,159,192
    G_LEGND_CANDY=*-GlyphData
    !byte 223,98,233
    !byte 160,160,160
    !byte 105,226,95
    G_LEGND_SOAP=*-GlyphData
    !byte 255,251,252
    !byte 160,160,160
    !byte 124,226,126
    G_WANNABE=*-GlyphData
    !byte 127,98,126
    !byte 17,17,97
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
!if *-MacroData >= $FF { !error "Out of MacroData memory" }

SIZEOF_TEXT=*-TextData

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

AINames:
    !scr "bd","jt","mg","rh"

;----------------------------------------------------------------------------
; MAX 2K ALLOWED HERE
;----------------------------------------------------------------------------
!byte 0 ; DUMMY to show where we are in report
!if * >= $1000 { !error "Out of memory" }
