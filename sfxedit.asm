; SFX editor

; TODO text in different color from digits

;constants
!addr SCREEN=$0400
!addr GETIN=$FFE4
!addr RowPtr=$4E                        ; Ptr to on screen Row
!addr Mask=$50
!addr ByteOffset=$51
!addr SfxOffset=$52
!addr RowNumber=$53
!addr RowCounter=$54
!addr SfxPtr=$55                        ; Sfx player integration frame pointer
!addr Tmp1=$56                          ; Sfx player integration temp data
!addr CursorX=$57                       ; X coordinate on screen (0..39)
!addr CursorY=$58                       ; Y row offset on screen (0..10)
!addr CursorTopRow=$59                  ; Top row# visible
!addr CursorTopOffset=$5A               ; Offset of top row# visible
!addr CursorPos=$5B                     ; Byte position of cursor in SfxData
!addr CursorRowMask=$5C                 ; Mask byte of row of cursor
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
        lda #' '
        sta SCREEN+256,x
        sta SCREEN+512,x
        sta SCREEN+768,x
        lda #TEXT_COLOR
        sta $D800,x
        sta $D900,x
        sta $DA00,x
        sta $DB00,x
        inx
        bne -

        lda #OFFSET_BYTES
        sta CursorX
        ldx #0
        stx CursorTopRow
        stx CursorTopOffset
        stx CursorY
        lda SfxData,x
        sta CursorRowMask
        lda #1                          ; after the mask
        sta CursorPos

RedrawMainLoop:
        jsr DrawScreen

MainLoop:
        lda #<(SCREEN+40*3)
        sta RowPtr
        lda #>(SCREEN+40*3)
        sta RowPtr+1
        lda CursorPos
        ldy #18
        jsr PutHex

        lda CursorY
        asl
        tay
        lda RowOffsets,y
        sta RowPtr
        lda RowOffsets+1,y
        sta RowPtr+1

        jsr InvertCursor
-       jsr GETIN
        beq -
        jsr InvertCursor

        ; DEBUG
        pha
        sta $0400+40+16
        jsr ToHex_AX
        sta $0400+40+17
        stx $0400+40+18
        pla
        ; /DEBUG

        ; TODO: you can move the cursor up or down but not before row 0 and not after the last row
        ; TODO: insert byte at cursor
        ; TODO: delete byte at cursor
        ; TODO: length always includes the terminating 0 (so minimum 1)
        ; TODO: offset (for multiple sounds in a single bank)
        ; TODO: fix bug empty row at the bottom
        ; TODO: fix bug SR=00 stops displaying

        ; $1D=RIGHT, $9D=LEFT, $91=UP, $11=DOWN, $85=F1, $86=F3, $87=F5, $88=F7, $89=F2..
        cmp #'-'
        beq DeleteRow
        cmp #'+'
        beq InsertRow
        cmp #'='                        ; PC keyboard has + on Shift-= so silently support that too
        beq InsertRow
        cmp #'.'
        beq DeleteByte
        cmp #20                         ; Delete
        beq DeleteByte
        cmp #' '
        beq DeleteByte
        cmp #$91
        beq MoveUp
        cmp #$11
        beq MoveDown
        cmp #$1D
        beq MoveRight
        cmp #$9D
        beq MoveLeft
        cmp #'0'
        bcc +
        cmp #'9'+1
        bcs +
        sec
        sbc #'0'                        ; 0..9
        jmp EditByte
+       cmp #'A'
        bcc +
        cmp #'F'+1
        bcs +
        sec
        sbc #'A'-10                     ; 10..15
        jmp EditByte
+       jmp MainLoop

DeleteRow:
        ldx #0                          ; TODO SfxData Row offset
        jsr DeleteSfxDataRow
        jmp RedrawMainLoop

InsertRow:
        ldx #0                          ; TODO SfxData Row offset
        lda #1                          ; Mask byte for single frame delay
        sta CursorRowMask
        jsr InsertSfxData
        jmp RedrawMainLoop

DeleteByte:
        inc $D020
        jmp DeleteByte

MoveUp:
        inc $D020
        jmp MoveUp

MoveDown:
        inc $D020
        jmp MoveDown

MoveLeft:
        ldx CursorX
        lda ColumnFlags,x
        sta Mask
        and #LEFT|LEFT2
        beq +                           ; not possible
        dex
        lda Mask
        and #LEFT2
        beq +
        dex
        lda ColumnFlags,x               ; is the cursor going to a byte?
        and #BITS
        tay
        lda CursorRowMask
        and BitFlags,y
        beq +                           ; nope
        dec CursorPos
+       stx CursorX
        jmp MainLoop

MoveRight:
        ldx CursorX
        lda ColumnFlags,x
        sta Mask
        and #RIGHT|RIGHT2
        beq +                           ; not possible
        inx
        lda Mask
        and #RIGHT2
        beq +
        inx
        lda Mask                        ; is the cursor on a byte?
        and #BITS
        tay
        lda CursorRowMask
        and BitFlags,y
        beq +                           ; nope
        inc CursorPos
+       stx CursorX
        jmp RedrawMainLoop              ; TODO hack to redraw updated byte

; A=0..15
EditByte:
        sta ByteOffset                  ; value to update
        ldx CursorX
        lda ColumnFlags,x
        and #BITS
        tay
        lda CursorRowMask
        and BitFlags,y
        bne +                           ; byte exists, so continue
        ; TODO insert fresh byte, update mask and continue
        jmp MainLoop
+       lda ColumnFlags,x
        and #HIGH
        beq +                           ; not set, update LOW
        ; update high nibble
        asl ByteOffset
        asl ByteOffset
        asl ByteOffset
        asl ByteOffset
        ldx CursorPos
        lda SfxData,x
        and #$0F
        ora ByteOffset
        sta SfxData,x
        ; TODO redraw current line/byte
++      jmp MoveRight
+       ; update low nibble
        ldx CursorPos
        lda SfxData,x
        and #$F0
        ora ByteOffset
        sta SfxData,x
        ; TODO redraw current line/byte
        jmp MoveRight


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
+       tax                         ; X=low nibble
        pla                         ; restore A
        lsr
        lsr
        lsr
        lsr
        ora #$30
        cmp #58
        bmi +
        sbc #57                     ; A=high nibble
+       rts

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
+       sta (RowPtr),y              ; high nibble
        iny
        pla                         ; restore A
        and #$0F
        ora #$30
        cmp #58
        bmi +
        sbc #57
+       sta (RowPtr),y              ; low nibble
        iny
        rts

; inverts cursor location (RowPtr+CursorX) (clobbers Y)
InvertCursor:
        pha
        ldy CursorX
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
; draw row# A starting at offset Y in the SfxData at RowPtr (clobbers A,X,Y,ByteOffset,Mask,SfxOffset) returns SfxOffset
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
        beq .rowend
        ; draw 7 bytes
.rowbytes:
        lda #OFFSET_BYTES
-       sta ByteOffset
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
+       ldy SfxOffset
        lda SfxData,y
        inc SfxOffset
        ldy ByteOffset
        jsr PutHex
++      lda ByteOffset
        clc
        adc #BYTESPACING
        cmp #OFFSET_BYTES + 7*BYTESPACING
        bne -
        rts
; draw end line
.rowend:
        ldx #LINEWIDTH
        ldy #OFFSET_BYTES
        lda #' '
-       sta (RowPtr),y
        iny
        dex
        bpl -
        rts

DrawEmptyRow:
        ldy #0
        lda #' '
-       sta (RowPtr),y
        iny
        cpy #FULLLINEWIDTH
        bne -
        rts


;----------------------------------------------------------------------------
; DRAW SCREEN
;----------------------------------------------------------------------------

ROWPTR=SCREEN+6*40
TOTALROWS=17
; draws screen (clobbers A,X,Y,RowCounter,Rownumber) (calls DrawRow, DrawEmptyRow)
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
+       ldy SfxOffset
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
; DATA MANAGEMENT
;----------------------------------------------------------------------------

; inserts single byte A at position X of the SfxData (clobbers A,X,ByteOffset)
InsertSfxData:
        pha                             ; store A
        stx ByteOffset
        ldx #$FF
-       cpx ByteOffset
        beq +                           ; done
        dex
        lda SfxData,x
        sta SfxData+1,x
        jmp -
+       pla
        sta SfxData,x
        rts

; deletes SfxRow at position X of the SfxData (clobbers A,X,Y,Mask)
DeleteSfxDataRow:
        txa
        tay                             ; Y=destination offset
        lda SfxData,x
        beq ++                          ; do nothing on last row of sfx
        and #$FE                        ; max 7 bytes are stored
        sta Mask
        ; calculate source offset
        inx                             ; count mask byte as well
-       asl
        beq +                           ; last bit shifted
        bcc -
        inx
        jmp -
+       bcc .copy
        inx
.copy:  lda SfxData,x
        sta SfxData,y
        iny
        inx
        bne .copy
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
; CURSOR DATA
;----------------------------------------------------------------------------

RowOffsets:
        !for i,0,24 { !word SCREEN + (i+6)*40 }

HIGH=$80
LOW=$00
LEFT=$40
LEFT2=$20
RIGHT=$10
RIGHT2=$08
BITS=$07
ColumnFlags:
        !fill OFFSET_BYTES,0 ; nothing here
        !byte HIGH |         RIGHT  | 0
        !byte LOW  | LEFT  | RIGHT2 | 0
        !byte 0
        !byte HIGH | LEFT2 | RIGHT  | 1
        !byte LOW  | LEFT  | RIGHT2 | 1
        !byte 0
        !byte HIGH | LEFT2 | RIGHT  | 2
        !byte LOW  | LEFT  | RIGHT2 | 2
        !byte 0
        !byte HIGH | LEFT2 | RIGHT  | 3
        !byte LOW  | LEFT  | RIGHT2 | 3
        !byte 0
        !byte HIGH | LEFT2 | RIGHT  | 4
        !byte LOW  | LEFT  | RIGHT2 | 4
        !byte 0
        !byte HIGH | LEFT2 | RIGHT  | 5
        !byte LOW  | LEFT  | RIGHT2 | 5
        !byte 0
        !byte HIGH | LEFT2 | RIGHT  | 6
        !byte LOW  | LEFT           | 6
        !fill ColumnFlags+40-*,0 ; nothing here

BitFlags:;      0   1   2   3   4   5   6
        !byte $80,$40,$20,$10,$08,$04,$02,$01


;----------------------------------------------------------------------------
ScreenData:
            ;     1234567890123456789012345678901234567890
            !scr "sfxedit---------------------------------"
            !scr "f1:play/stop           .:clr +:ins -:del"
            !scr "                                        "
            !scr " offset ??    pos ?? length ??          "
            !scr "                                        "
            !scr " row mask fl fh pl ph wv ad sr          "
            !fill ScreenData+256-*,' '
            ; !scr " ??   ??  ?? ?? ?? .. .. ?? ??          "
            ; !scr " ??   ??  ?? ?? .. .. ?? .. ..          "
            ; !scr " ??   ??  .. .. .. .. .. .. ..          "
            ; !scr " ??   ??  .. .. .. .. .. .. ..          "
            ; !scr " ??   ??  .. .. .. .. .. .. ..          "
            ; !scr "                                        "
            ; !scr "                                        "
            ; !scr "                                        "
            ; !scr "                                        "
            ; !scr "                                        "
            ; !scr "                                        "
            ; !scr "                                        "
            ; !scr "                                        "
            ; !scr "                                        "
            ; !scr "                                        "
            ; !scr "                                        "
            ; !scr "                                        "
            ; !scr "                                        "
            ; !scr "                                        "


;----------------------------------------------------------------------------
; Actually edited sound data
;----------------------------------------------------------------------------
*=$1000
SfxData:
            ;     Bitmask,   FL, FH, PL, PH, WV, AD, SR
            !byte %11111110,$0A,$4D,$00,$08,$81,$22,$F2
            !byte 1,1
            !byte %00001000,                $80
            !byte 1,1
SFX_TEST=*-SfxData
            !byte %11001000,$0A,$4D,        $81
            !byte 1,1,1,1,1
            !byte %11001000,$85,$46,        $80
SFX_NONE=*-SfxData
            !byte 0

            !fill 256,0
