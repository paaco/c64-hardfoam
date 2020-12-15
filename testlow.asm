; Quick scan to see what part of <$0400 you can also use since
; the contest is limited to ONLY the first 2K of memory <$1000

*=$0801
!byte $0c,$08,$b6,$07,$9e,$20,$32,$30,$36,$32,$00,$00,$00   ; 1974 SYS 2062

;  Used by Exomizer: Depacker - main code ($0100 - $01BA)
; The stack is filled backwards from $1FF down to $100.
; Apparently there's not really a lot of recursion going on, so quite some room here

; This seems safe to use:
; $0200-$0258/512-600:   BASIC Input Buffer (Input Line from Screen)

; Maybe changing this vvv randomly fcks with keyboard read?
;   $0259-$0262/601-610:   Active logical File numbers
;   $0263-$026C/611-620:   Active File First Addresses (Device numbers)
;   $026D-$0276/621-630:   Active File Secondary Addresses

; KEEP:  $0277-$0280/631-640:   Keyboard Buffer Queue (FIFO)
;  some more keyboard stuff

; $0293-$02FF seems same (mostly RS232 stuff)
;  $0293/659:   RS232 Pseudo 6551 control Register Image

; KEEP: $0300-$0333 IRQ vectors

; $0334-$03FF seems safe to use (tape buffer and empty)


;2062
start:
        ldx #0
-       lda #1
        sta $D800,x
        sta $DA00,x
        lda #2
        sta $D900,x
        sta $DB00,x
        inx
        bne -
-
        lda $0000,x
        sta $0400,x
        lda $0100,x
        sta $0500,x
        lda $0200,x
        sta $0600,x
        lda $0300,x
        sta $0700,x
        inx
        bne -
        tsx
        stx $0400
        jsr $ffe4
        jmp start