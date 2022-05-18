; SFX editor

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
!addr CursorRowOffset=$5D               ; Byte position of mask on row of cursor
!addr SoundPlaying=$5E                  ; flag for sound enabled, doubles as repeat delay
!addr SoundRestart=$5F                  ; place to restart sound
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
!byte $0b,$08,<1974,>1974,$9e,$32,$30,$36,$31,$00,$00,$00

;----------------------------------------------------------------------------
Start:
        ldx #0
        stx $D021
        stx SoundPlaying
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
        stx CursorRowOffset
        stx CursorY
        lda SfxData,x
        sta CursorRowMask
        lda #1                          ; after the mask
        sta CursorPos

        ; init SID
        lda #$00
        sta $D415                       ; Filter Cutoff Frequency: Low-Nybble
        lda #$00
        sta $D416                       ; Filter Cutoff Frequency: High-Byte
        lda #%00000000                  ; RRRRX321
        sta $D417                       ; Filter Resonance Control / Voice Input Control
        lda #%00000000                  ; 3HBLVVVV
        sta $D418                       ; Select Filter Mode and Volume
            ;override from hardfoam
            lda #20                     ; cutoff high
            sta $D416
            lda #%00111111              ; RRRRE321 reso + filter voices
            sta $D417
            lda #%00111111              ; 3HBLVVVV band + volume
            sta $D418

        ; setup IRQ
        sei
        lda #$7f
        sta $DC0D                       ; disable timer interrupts from CIA chips
        sta $DD0D                       ; (used by KERNAL keyboard scanner though)
        lda $DC0D                       ; ack pending timer interrupts from CIA chips
        lda $DD0D
        lda #$01
        sta $D01A
    RASTER=$FC
        lda #RASTER
        sta $D012
        lda #$1B + (RASTER>>8)*$80
        sta $D011
        lda $0314
        sta ContinueIRQ
        lda $0315
        sta ContinueIRQ+1
        lda #<SoundIRQ
        sta $0314
        lda #>SoundIRQ
        sta $0315
        cli

RedrawMainLoop:
        jsr DrawScreen

MainLoop:
        lda #<(SCREEN+40*3)
        sta RowPtr
        lda #>(SCREEN+40*3)
        sta RowPtr+1
        lda CursorTopOffset
        ldy #5
        jsr PutHex
        lda CursorRowOffset
        ldy #8
        jsr PutHex
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

        ; $1D=RIGHT, $9D=LEFT, $91=UP, $11=DOWN, $85=F1, $86=F3, $87=F5, $88=F7, $89=F2..
        cmp #$85                        ; F1
        bne +
--      jmp PlayStopSound
+       cmp #$8A                        ; F4 for some reason I keep pressing F4 to stop
        beq --
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
        bne +
        jmp MoveUp
+       cmp #$11
        bne +
        jmp MoveDown
+       cmp #$1D
        bne +
        jmp MoveRight
+       cmp #$9D
        bne +
        jmp MoveLeft
+       cmp #'0'
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
        ldx CursorRowOffset
        jsr DeleteSfxDataRow
        ldx CursorRowOffset
        lda SfxData,x
        sta CursorRowMask
        jsr UpdateCursorPos
        jmp RedrawMainLoop

InsertRow:
        ldx CursorRowOffset
        lda #$01                        ; Mask byte for single frame delay
        sta CursorRowMask
        jsr InsertSfxData
        jsr UpdateCursorPos
        jmp RedrawMainLoop

DeleteByte:
        ldx CursorX
        lda ColumnFlags,x
        and #BITS
        tay
        lda CursorRowMask
        and BitFlags,y
        beq ++                          ; no byte here, stop
        ; update mask
        eor CursorRowMask               ; add remaining flags back
        bne +
        lda #$01                        ; Mask byte for single frame delay
+       sta CursorRowMask
        ldx CursorRowOffset
        sta SfxData,x
        ldx CursorPos
        jsr DeleteSfxData
++      jmp MoveRight

MoveUp:
        lda CursorY
        bne .noscrollup
        ; scroll up to previouw row
        ldx CursorTopOffset
        beq ++                          ; can't go more up
        jsr GetRowBefore
        dec CursorTopRow
        stx CursorTopOffset
        jmp +
.noscrollup:
        ; move up to previous row
        dec CursorY
        ldx CursorRowOffset
        jsr GetRowBefore
+       stx CursorRowOffset
        lda SfxData,x
        sta CursorRowMask
        jsr UpdateCursorPos
++      jmp RedrawMainLoop

MoveDown:
        lda CursorY
        cmp #TOTALROWS-1
        bne .noscrolldown
        inc CursorTopRow
        ldx CursorTopOffset             ; top row
        lda SfxData,x
        ora #$01                        ; always skip a Mask byte
-       beq +                           ; continue
        asl
        bcc -
        inc CursorTopOffset
        jmp -
.noscrolldown:
        inc CursorY
+       lda CursorRowMask               ; bottom row
        ora #$01                        ; always skip a Mask byte
-       beq +
        asl
        bcc -
        inc CursorRowOffset
        jmp -
+       ldx CursorRowOffset
        lda SfxData,x
        sta CursorRowMask
        jsr UpdateCursorPos
        jmp RedrawMainLoop

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
+       stx CursorX
        jsr UpdateCursorPos
tomainloop:
        jmp MainLoop

MoveRight:
        ldx CursorX
        lda ColumnFlags,x
        sta Mask
        and #RIGHT|RIGHT2
        beq tomainloop                  ; not possible
        inx
        lda Mask
        and #RIGHT2
        beq +
        inx
+       stx CursorX
        jsr UpdateCursorPos
        jmp RedrawMainLoop              ; TODO only redraw current row

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
        ; insert fresh byte at CursorPos
        ldx CursorPos
        lda #0                          ; fresh byte
        jsr InsertSfxData
        ; update mask
        lda CursorRowMask
        ora BitFlags,y
        and #$FE                        ; last bit is unused when register data is present
++      sta CursorRowMask
        ldx CursorRowOffset
        sta SfxData,x
        ; continue
        ldx CursorX
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
++      jmp MoveRight
+       ; update low nibble
        ldx CursorPos
        lda SfxData,x
        and #$F0
        ora ByteOffset
        sta SfxData,x
        jmp MoveRight

PlayStopSound:
        lda SoundPlaying
        bne +
        ; start sound
        ldx CursorRowOffset
        jsr FindSfxStart
        stx SoundRestart
        stx SfxPtr
        lda #%00001111                  ; full volume, no filtering
            ; override from hardfoam
            lda #%00101111              ; 3HBLVVVV band + volume
        sta $D418
        inc SoundPlaying
        jmp MainLoop
+       sei
        lda #0
        sta SoundPlaying                ; stop
        sta $D418                       ; kill sound
        cli
        jmp MainLoop


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

; determine CursorPos from CursorRowOffset, CursorRowMask and CursorX (clobbers A,X,Y,Mask,ByteOffset)
UpdateCursorPos:
        ldx CursorX
        lda ColumnFlags,x
        and #BITS
        sta ByteOffset                  ; 0..6
        ldx CursorRowOffset             ; start at the mask
        inx                             ; plus 1
        lda CursorRowMask
        sta Mask
        beq +
        lda ByteOffset
        beq +
-       asl Mask
        bcc ++
        inx                             ; count bit
++      dec ByteOffset
        bne -
+       stx CursorPos
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
; draw row RowNumber starting at SfxOffset in SfxData at RowPtr (clobbers A,X,Y,ByteOffset,Mask,SfxOffset) increases SfxOffset
DrawRow:
        lda RowNumber
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
TOTALROWS=18
; draws screen (clobbers A,X,Y,RowCounter,Rownumber) (calls DrawRow, DrawEmptyRow)
DrawScreen:
        lda #<ROWPTR
        sta RowPtr
        lda #>ROWPTR
        sta RowPtr+1

        lda #TOTALROWS
        sta RowCounter

        lda CursorTopRow
        sta RowNumber
        ldy CursorTopOffset
        sty SfxOffset
-       jsr DrawRow
        lda RowPtr
        clc
        adc #40
        sta RowPtr
        bcc +
        inc RowPtr+1
+       inc RowNumber
        dec RowCounter
        bne -
        rts


;----------------------------------------------------------------------------
; DATA MANAGEMENT
;----------------------------------------------------------------------------

; inserts single byte A at position X of the SfxData (clobbers X,Mask)
InsertSfxData:
        pha                             ; store A
        stx Mask
        ldx #$FF
-       cpx Mask
        beq +                           ; done
        dex
        lda SfxData,x
        sta SfxData+1,x
        jmp -
+       pla
        sta SfxData,x
        rts

; deletes byte at position X of the SfxData (clobbers X)
DeleteSfxData:
-       lda SfxData+1,x
        sta SfxData,x
        inx
        cpx #$FF
        bne -
        lda #0                          ; don't read outside the page
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

; determine offset of row before X in SfxData (clobbers A,X,ByteOffset,SfxOffset) returns X
GetRowBefore:
        stx ByteOffset                  ; this is where we want to end
        stx SfxOffset
        cpx #0
        beq ++                          ; no rows before
        ldx #0
--      stx SfxOffset                   ; "row before" offset
        lda SfxData,x                   ; Mask byte (always there since ByteOffset>0)
        and #$FE                        ; max 7 bytes are stored
        inx
-       cmp #0
        beq +
        asl
        bcc -
        inx
        bne -
+       cpx ByteOffset
        bne --
++      ldx SfxOffset
        rts

; determine start offset of sound from rowoffset X in SfxData (clobbers A,X) returns X
FindSfxStart:
        ldx #0                          ; TODO determine real start, for now, just support 1 sound
        rts


;----------------------------------------------------------------------------
; IRQ
;----------------------------------------------------------------------------

SOUND_DELAY=50
SoundIRQ:
        pha
        txa
        pha
        tya
        pha
        asl $d019                       ; acknowledge raster irq
        dec $D020
        lda SoundPlaying
        beq ++
        jsr PlaySoundFx
        ; timed restart
        ldy SfxPtr
        lda SfxData,y
        bne ++
        inc SoundPlaying
        lda SoundPlaying
        cmp #SOUND_DELAY                ; #frames between sounds
        bne ++
        ; restart after N frames of nothingness
        lda #1
        sta SoundPlaying
        lda SoundRestart
        sta SfxPtr
++      inc $D020
        pla
        tay
        pla
        tax
        pla
ContinueIRQ=*+1
        jmp $0000                       ; SELF-MODIFIED


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
            !scr " off ??/??    pos ??                    "
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
            !byte %00000000,$00,$00,$00,$00,$00,$00,$00
            !fill SfxData+256-*,0
