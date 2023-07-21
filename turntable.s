; constants
SLOTn0 = $60 ; assumes slot 6

; disk soft switches, indexed by SLOTn0
PHASE0OFF   = $C080+SLOTn0
PHASE0ON    = $C081+SLOTn0
PHASE1OFF   = $C082+SLOTn0
PHASE1ON    = $C083+SLOTn0
PHASE2OFF   = $C084+SLOTn0
PHASE2ON    = $C085+SLOTn0
PHASE3OFF   = $C086+SLOTn0
PHASE3ON    = $C087+SLOTn0
MOTOROFF    = $C088+SLOTn0
MOTORON     = $C089+SLOTn0
DISK1SELECT = $C08A+SLOTn0
DISK2SELECT = $C08B+SLOTn0
SHIFT       = $C08C+SLOTn0
READ        = $C08E+SLOTn0
WRITE       = $c08F+SLOTn0


MOTOROFFBASE = $c088
MOTORONBASE = $c089
SHIFTBASE = $C08C
LOADBASE = $c08D
READBASE = $c08e
WRITEBASE = $C08F

; zero page
; TODO: use unused addresses
PHASE1STATE = $00 ; .. $01
ZPDUMMY = $02
loopctr = $03 ; .. $04

LOOP_OUTER = 120

; internal buffers
STEP_TABLE = $4000
PHASE1STATE_TABLE = $4100

WAIT = $FCA8

.proc main
    ; spin up disk
	LDA DISK1SELECT
	LDA MOTORON

; STEP_TABLE:
;    86, 86, 86, 86  ; Phase 3 off   X . . . ; track 0
;    83, 83, 83, 83  ; Phase 1 on    X X . . ; track 0.25
;    80, 80, 80, 80  ; Phase 0 off   . X . . ; track 0.5
;    85, 85, 85, 85  ; Phase 2 on    . X X . ; track 0.75
;    82, 82, 82, 82  ; Phase 1 off   . . X . ; track 1
;    87, 87, 87, 87  ; Phase 3 on    . . X X ; track 1.25
;    84, 84, 84, 84  ; Phase 2 off   . . . X ; track 1.5
;    81, 81, 81, 81  ; Phase 0 on    X . . X ; track 1.75
;    [... repeat 8 times to fill 256 bytes ...]

make_step_table:
	; start with PHASE3OFF
	LDA #<PHASE3OFF
    ;CLC
    ;ADC #SLOTn0

	LDX #$00
@0:
	STA STEP_TABLE,X
	INX
	STA STEP_TABLE,X
	INX
	STA STEP_TABLE,X
	INX
	STA STEP_TABLE,X
	INX
	beq @done
	SEC ; TODO: could avoid by assuming carry always set and compensating in the ADC
	SBC #$03
	CLC
	AND #$07
	ADC #(SLOTn0+$80)
	BNE @0

@done:

; PHASE1STATE_TABLE ;
;    83, 83, 83, 83 ; [0  ..  3] phase 1 off
;    82, 82, 82, 82 ; [4  ..   ] phase 1 on
;    83, 83, 83, 83 ; [   ..   ] phase 1 on
;    83, 83, 83, 83 ; [   .. 15] phase 1 on
;    82, 82, 82, 82 ; [16 ..   ] phase 1 off
;    82, 82, 82, 82 ; [   ..   ] phase 1 off
;    82, 82, 82, 82 ; [   ..   ] phase 1 off
;    82, 82, 82, 82 ; [   .. 31] phase 1 off

make_phase1_state_table:
    LDX #$00
    LDY #<PHASE1OFF
@0:
    TXA
    AND #31
    CMP #$4
    BNE @1
    LDY #<PHASE1ON
    BNE @next
@1:
    CMP #$10
    BNE @next
    LDY #<PHASE1OFF

@next:
    TYA
    STA PHASE1STATE_TABLE,X

    INX
    BNE @0

@done:

; Step to track 0
    LDY #$80 ; current half-track count
    LDX #$00
@seek0:
    LDA PHASE0OFF,X
    TYA
    AND #$03
    ASL
    TAX
    LDA PHASE0ON,X
    LDA #$56
    JSR WAIT
    DEY
    BPL @seek0

; Prepare to begin
prepare:
    ; enable phase 0
    LDA PHASE0ON
    LDA PHASE1OFF
    LDA PHASE2OFF
    LDA PHASE3OFF

    ; settle
    LDA #$FF
    JSR WAIT
    LDA #$FF
    JSR WAIT
    LDA #$FF
    JSR WAIT
    LDA #$FF
    JSR WAIT
    LDA #$FF
    JSR WAIT
    LDA #$FF
    JSR WAIT
    LDA #$FF
    JSR WAIT
    LDA #$FF
    JSR WAIT
    LDA #$FF
    JSR WAIT
    LDA #$FF
    JSR WAIT
    LDA #$FF
    JSR WAIT
    LDA #$FF
    JSR WAIT
    LDA #$FF
    JSR WAIT

    ; phase 1 is off to begin with
    LDA #>PHASE1OFF
    STA PHASE1STATE+1
    LDA #<PHASE1OFF
    STA PHASE1STATE

    LDA #$00
    STA loopctr

    JMP prepare_read

    LDY #$60
    
    LDA LOADBASE,Y
    LDA READBASE,Y
    BPL @noerror
    BRK

@noerror:
    STA WRITEBASE,Y 
    CMP SHIFTBASE,Y
    STA ZPDUMMY
    NOP
    NOP
    NOP
    NOP
    NOP
    NOP
    

prepare_write:
    NOP
    ; start writing 40-cycle FF sync bytes
    LDA #$FF
    LDY #$60

    LDX #$FF  ; number of sync bytes to write
    STA LOADBASE,Y 
    CMP SHIFTBASE,Y

; 40 - 9 = 31 cycles
    STA ZPDUMMY
    NOP
@0:
    NOP
    NOP
    NOP
    NOP
    NOP
    NOP
    NOP
    NOP
    NOP
    NOP
    NOP
    NOP
    NOP

    STA LOADBASE,Y
    CMP SHIFTBASE,Y
    DEX
    BNE @0

    ; write header
    NOP

    ; make sure this is a FF40 too
    NOP
    NOP
    NOP
    NOP

    LDA #$D5
    JSR write_nibble9 ; 6 + 9 + 
    ; 9 + 6
    LDA #$AA ; 2
    JSR write_nibble9 ; 6 + 9

	; start outer loop, counts from (LOOP_OUTER-1) .. 0
	LDX #LOOP_OUTER

    ; pad to 32 cycles until the next disk STA/CMP in write_nibble
    STA ZPDUMMY
    NOP
    NOP

disk_write_loop:
    ; need to turn off phase 1 around all writes because the Disk II hardware
    ; suppresses writes if it is enabled
	LDA PHASE1OFF ; 4 ; don't interfere with writes

	LDA #$FF ; 2
	LDY #$60 ; 2 XXX
	STA LOADBASE,Y ; 5
	CMP SHIFTBASE,Y ; 4

    ; reassert the current phase 1 state
    LDY #$00 ; 2 XXX
	LDA (PHASE1STATE),Y ; 5 Y=0

    STA ZPDUMMY    

	DEX ; 2
	BNE write_nibble ; 2/3

    ; falls through when it is time to step the head
    ; 31 cycles so far, need 33 to get back on 32-cycle write cadence

	; X=0
	INC loopctr ; 5
	LDY loopctr ; 3
	LDX STEP_TABLE,Y ; 4
	LDA $C000,X ; 4 toggle next phase switch

    LDX PHASE1STATE_TABLE,Y ; 4
	STX PHASE1STATE ; 3

	LDX #LOOP_OUTER ; 2 reset inner loop counter1

    STA ZPDUMMY
    NOP
	BNE disk_write_loop ; 3 always

write_nibble9:
    CLC ; 2
    PHA ; 3
    PLA ; 4
    STA LOADBASE,Y ; 5
    ORA SHIFTBASE,Y ; 4
    RTS ; 6

prepare_read:
	; start outer loop, counts from (LOOP_OUTER-1) .. 0
	LDX #LOOP_OUTER
    LDA READ

    LDY #$60 ; XXX

@loop:
    ldx #$5
@read:
    lda SHIFTBASE,y
    bpl @read
    INC $600
    STA $601
    cmp #$FF
    bne @loop
    dex
    bne @read

	LDX #LOOP_OUTER ; XXX

    ; sync to track start
@startsync:
    LDA SHIFTBASE,Y ; XXX
    BPL @startsync
    STA $400
@tryd5:
    EOR #$D5
    BNE @startsync

@tryaa:
    LDA SHIFTBASE,Y ; 4
    BPL @tryaa ; 2/3
    CMP #$AA ; 2
    STA $401 ; 4
    BNE @tryd5 ; 2/3
    ; 14 cycles

    ; Now do 33 cycle reads until we get a 0 indicating that we have
    ; waited too long and the read register has been cleared

    NOP    
@0: ; 17 more cycles
    STA ZPDUMMY
    INC $700 ; 6
    NOP
    NOP    
    NOP
    NOP
    
    LDA SHIFTBASE,y ; 4
    STA $701 ; 4
    ; 5
    STA ZPDUMMY
    NOP
    BNE @0 ; 3

    ; we have just missed the window by 1 cycle, so do a 29 cycle read
    ; this time to get back in sync
    ; 9 cycles so far

    STA ZPDUMMY
    NOP

    ; plus 5 following
    ; XXX consume remaining BB markers


; 31 cycles in the common case
disk_read_loop:
    LDA SHIFT ; 4
    ; should normally fall through, will occasionally loop once when
    ; we have slipped a cycle and the nibble is not ready after 31
    ; cycles
    BPL disk_read_loop ; 2/3

buffer:
    STA $5000,X ; 5
    LDA buffer+2 ; 4
    CMP #$80 ; 2
    BEQ done ; 2

    STA ZPDUMMY
    NOP
    NOP

	DEX ; 2
	BNE disk_read_loop ; 2/3

    ; falls through when it's time to step the head
    ; 30 cycles so far, need 32 to get back on 31-cycle cadence

	INC loopctr ; 5
	LDY loopctr ; 3
	LDX STEP_TABLE,Y ; 4
	LDA $C000,X ; 4 toggle next phase switch

    INC buffer+2 ; 6

    ; LDX PHASE1STATE_TABLE,Y ; 4
	; STX PHASE1STATE ; 3

    STA ZPDUMMY
    NOP

	LDX #LOOP_OUTER ; 2 reset inner loop counter1
	BNE disk_read_loop ; 3 always

done:
    STA MOTOROFF
    BRK

;read_nibble:
;	NOP
;	NOP
;	NOP
;	NOP
;	LDY Q6L ; 4 read
;	BIT decodetable,Y ; 4 [SMC] - increment table to step through tracks
;	BEQ end_of_track ; 2
;	BPL notick ; 2/3
;	STA $c030 ; 4
;	JMP diskloop ; 3
;	
;notick:
;	;2+3+4+4+3+3+3
;	STA dummy ; 3
;   JMP diskloop ; 3

; phase path:
; 2+2+5+3+4+4+2+2+2+3+3 = 32

.endproc