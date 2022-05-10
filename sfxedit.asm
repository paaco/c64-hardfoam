; SFX editor

; TODO text in different color from digits
; TODO implement +
; TODO implement -
; TODO implement row edit

;constants
!addr SCREEN=$0400
!addr GETIN=$FFE4
!addr RowPtr=$4E                        ; Ptr to on screen Row
!addr CursorOff=$50                     ; Offset on current screen to invert cursor at
!addr Mask=$51
!addr ByteOffset=$52
!addr SfxOffset=$53
!addr RowNumber=$54
!addr RowCounter=$55
!addr SfxPtr=$56                        ; Sfx player integration frame pointer
!addr Tmp1=$57                          ; Sfx player integration temp data
; looks like we have room at least till $70 (floating point temp storage)

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
TEXT_COLOR=CYAN

*=$0801
!byte $0c,$08,<1974,>1974,$9e,$32,$30,$36,$31,$00,$00,$00

;----------------------------------------------------------------------------
Start:
        ldx #0
        stx $D021
        lda #DARK_GREY
        sta $D020
-       lda ScreenData,x
        sta SCREEN,x
        lda ScreenData+256,x
        sta SCREEN+256,x
        lda ScreenData+512,x
        sta SCREEN+512,x
        lda ScreenData+768,x
        sta SCREEN+768,x
        lda #TEXT_COLOR
        sta $D800,x
        sta $D900,x
        sta $DA00,x
        sta $DB00,x
        inx
        bne -

        jsr DrawScreen

        lda #<ROWPTR
        sta RowPtr
        lda #>ROWPTR
        sta RowPtr+1
        lda #OFFSET_BYTES
        sta CursorOff

MainLoop:
        jsr InvertCursor
-       jsr GETIN
        beq -
        jsr InvertCursor

        ; DEBUG
        pha
        sta $0400+40+20
        jsr ToHex_AX
        sta $0400+40+22
        stx $0400+40+23
        pla
        ; /DEBUG

        ; TODO: - on an existing row except the 0, deletes it and redraws the screen
        ; TODO: + on any row, inserts a new one before it and redraws the screen
        ; TODO: you can move the cursor up or down but not before row 0 and not after the last row
        ; TODO: you can move the cursor left or right over the digits (it skips the spaces in between)
        ; TODO: you can't move the cursor left or right over the last row (always moves to first column)
        ; TODO: the sound always ends with a 0
        ; TODO: length always includes the terminating 0 (so minimum 1)

        ; $1D=RIGHT, $9D=LEFT, $91=UP, $11=DOWN, $85=F1, $86=F3, $87=F5, $88=F7, $89=F2..
        cmp #'-'
        beq DeleteRow
        cmp #'+'
        beq InsertRow
        cmp #'='                        ; PC keyboard has + on Shift-= so silently support that too
        beq InsertRow

        jmp MainLoop

DeleteRow:
        inc $D020
        jmp DeleteRow

InsertRow:
        inc $D020
        jmp InsertRow


;----------------------------------------------------------------------------
; INPUT/OUTPUT functions
;----------------------------------------------------------------------------

; converts binary A to 2 hex screen characters in A/X (clobbers A,X)
ToHex_AX:
            pha                         ; store A
            and #$0F
            ora #$30
            cmp #58
            bmi +
            sbc #57
+           tax                         ; X=low nibble
            pla                         ; restore A
            lsr
            lsr
            lsr
            lsr
            ora #$30
            cmp #58
            bmi +
            sbc #57                     ; A=high nibble
+           rts

; puts binary A to 2 hex screen characters at RowPtr+Y (clobbers A,Y)
PutHex:
            pha                         ; store A
            lsr
            lsr
            lsr
            lsr
            ora #$30
            cmp #58
            bmi +
            sbc #57
+           sta (RowPtr),y              ; high nibble
            iny
            pla                         ; restore A
            and #$0F
            ora #$30
            cmp #58
            bmi +
            sbc #57
+           sta (RowPtr),y              ; low nibble
            iny
            rts

; inverts cursor location (clobbers Y)
InvertCursor:
            pha
            ldy CursorOff
            lda (RowPtr),y
            eor #$80
            sta (RowPtr),y
            pla
            rts


;----------------------------------------------------------------------------
; DRAW ROW
;----------------------------------------------------------------------------

OFFSET_ROWNR=1
OFFSET_MASK=6
OFFSET_BYTES=10
BYTESPACING=3
LINEWIDTH=19
FULLLINEWIDTH=31
; draw row# A starting at offset Y in the SfxData at RowPtr (clobbers A,X,Y,Mask,SfxOffset) returns SfxOffset
DrawRow:
            sty SfxOffset
            ldy #OFFSET_ROWNR
            jsr PutHex
            ldy SfxOffset
            lda SfxData,y
            inc SfxOffset
            sta Mask
            ldy #OFFSET_MASK
            jsr PutHex
            lda Mask
            beq .rowends

.rowbytes:  ; draw 7 bytes
            lda #OFFSET_BYTES
-           sta ByteOffset
            asl Mask
            bcs +
            ; draw empty
            lda #'.'
            ldy ByteOffset
            sta (RowPtr),y
            iny
            sta (RowPtr),y
            bne ++                      ; always
            ; draw number
+           ldy SfxOffset
            lda SfxData,y
            inc SfxOffset
            ldy ByteOffset
            jsr PutHex
++          lda ByteOffset
            clc
            adc #BYTESPACING
            cmp #OFFSET_BYTES + 7*BYTESPACING
            bne -
            rts

.rowends:   ; draw end line
            ldx #LINEWIDTH
            ldy #OFFSET_BYTES
            lda #'='
-           sta (RowPtr),y
            iny
            dex
            bpl -
            rts

DrawEmptyRow:
            ldy #0
            lda #' '
-           sta (RowPtr),y
            iny
            cpy #FULLLINEWIDTH
            bne -
            rts


;----------------------------------------------------------------------------
; DRAW SCREEN
;----------------------------------------------------------------------------

ROWPTR=SCREEN+6*40
TOTALROWS=17
; draws screen (clobbers A,X,Y,RowCounter,Rownumber,SfxOffset)
DrawScreen:
        lda #<ROWPTR
        sta RowPtr
        lda #>ROWPTR
        sta RowPtr+1

        lda #TOTALROWS
        sta RowCounter

        lda #0                          ; top row#
        sta RowNumber
        ldy #0                          ; top sound offset
        sty SfxOffset
-       ldy SfxOffset
        lda RowNumber
        jsr DrawRow

        ; advance row
        lda RowPtr
        clc
        adc #40
        sta RowPtr
        bcc +
        inc RowPtr+1
+
        ldy SfxOffset
        lda SfxData-1,y
        beq .drawempty

        inc RowNumber
        dec RowCounter
        bne -

.drawempty:
        lda RowCounter
        beq ++
-       jsr DrawEmptyRow
        ; advance row
        lda RowPtr
        clc
        adc #40
        sta RowPtr
        bcc +
        inc RowPtr+1
+       dec RowCounter
        bne -
++      rts



;----------------------------------------------------------------------------
; (FRAME) AUDIO PLAYER
;----------------------------------------------------------------------------

PlaySoundFx:
            ldx #0              ; SID register counter
            ldy SfxPtr
            lda SfxData,y       ; get bit counter
            beq ++              ; 0=done
            inc SfxPtr
            sta Tmp1            ; bit counter
-           asl Tmp1
            bcc +               ; bit unset, skip to next
            ldy SfxPtr
            lda SfxData,y
            inc SfxPtr
            sta $D400,x
+           inx
            cpx #7
            bne -
++          rts


;----------------------------------------------------------------------------
ScreenData:
            ;     1234567890123456789012345678901234567890
            !scr "sfxedit---------------------------------"
            !scr "f1:play/stop                 +:ins -:del"
            !scr "                                        "
            !scr " offset ??           length ??          "
            !scr "                                        "
            !scr " row mask fl fh pl ph wv ad sr          "
            !scr " ??   ??  ?? ?? ?? .. .. ?? ??          "
            !scr " ??   ??  ?? ?? .. .. ?? .. ..          "
            !scr " ??   ??  .. .. .. .. .. .. ..          "
            !scr " ??   ??  .. .. .. .. .. .. ..          "
            !scr " ??   ??  .. .. .. .. .. .. ..          "
            !scr "                                        "
            !scr "                                        "
            !scr "                                        "
            !scr "                                        "
            !scr "                                        "
            !scr "                                        "
            !scr "                                        "
            !scr "                                        "
            !scr "                                        "
            !scr "                                        "
            !scr "                                        "
            !scr "                                        "
            !scr "                                        "
            !scr "                                        "


;----------------------------------------------------------------------------
; Actually edited sound data
;----------------------------------------------------------------------------
*=$1000
SfxData:
;             ;     Bitmask,   FL, FH, PL, PH, WV, AD, SR
;             !byte %11111110,$0A,$4D,$00,$08,$81,$22,$F2
;             !byte 1,1
;             !byte %00001000,                $80
;             !byte 1,1
; SFX_TEST=*-SfxData
;             !byte %11001000,$0A,$4D,        $81
;             !byte 1,1,1,1,1
;             !byte %11001000,$85,$46,        $80
; SFX_NONE=*-SfxData
;             !byte 0

            !fill 256,0
