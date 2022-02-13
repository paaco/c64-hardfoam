; jump table test

*=$C000

!addr JumpAddr=$FE ; saves 2 bytes when setting up jump
; Hashed effect table X=effect A=phase (single bit)
    ldx #E_ALL_GAIN11
    lda #%00001000
    and EffectTable+2,x
    beq .nothing
    lda EffectTable,x
    sta JumpAddr
    lda EffectTable+1,x
    sta JumpAddr+1
    jmp (JumpAddr)
.nothing: rts

; TODO Effects interface:
;  how to access the current card
;  how to affect all (other/own) cards

Effect_All_Gain11:
    cmp #%00001000
    bne +
    ; phase 1
    rts
+   ; phase 2
    rts

Effect_None:
    rts

E_ALL_GAIN11=20
T_NONE=30

!align $FF,0
EffectTable:
JumpTable:
    !fill 256,0
*=JumpTable+E_ALL_GAIN11,overlay
    !word Effect_All_Gain11,%00001100 ; phases it applies to
*=JumpTable+T_NONE,overlay
    !word T_NONE,%00001100 ; phases it applies to
