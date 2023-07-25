; constants
SLOTn0 = $60 ; assumes slot 6

; how many nibbles to write before stepping head
LOOP_INNER = 120

; disk soft switches, pre-indexed by SLOTn0
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

; internal buffers
STEP_TABLE1 = $4000
STEP_TABLE2 = $4100
PHASE1STATE_TABLE = $4200
data = $4300

WAIT = $FCA8

.proc main
    ; spin up disk
    LDA DISK1SELECT
    LDA MOTORON

    JMP make_step_table

; STEP_TABLE: (entries have SLOTn0 added)

STEP_TABLE1_DATA:
; XXX use PHASE1OFF_BASE etc
; XXX constants for sentinels
.byte    $86, $86, $86, $86  ; X . . . ; track 0
.byte    $81, $80, $81, $80  ; X X . . ; track 0.25
.byte    $80, $80, $80, $80  ; . X . . ; track 0.5
.byte    $85, $84, $85, $82  ; . X X . ; track 0.75 XXX 82/84 swapped so we can use 84 as a sentinel
.byte    $82, $82, $82, $82  ; . . X . ; track 1
.byte    $87, $87, $87, $87  ; . . X X ; track 1.25
.byte    $84, $84, $84, $84  ; . . . X ; track 1.5
.byte    $81, $81, $81, $81  ; X . . X ; track 1.75

STEP_TABLE2_DATA:
; values are the same as STEP_TABLE1_DATA except when:
; - the next batch will turn phase 1 on ($89 = MOTORON is used as a sentinel for this)
; - we need to turn phase 1 on and off on alternating batches,
;   which may flipping two different switches
; - XXX 84 is also used as a sentinel value to break out of push/pull
; - we make sure all of the 83 are in this table so we can filter for them in make_phase1_state_table
.byte    $86, $86, $86, $89  ; X . . . ; track 0 ; 89 is a sentinel to enter the push/pull mode when phase 1 is active
.byte    $83, $82, $83, $82  ; X X . . ; track 0.25
.byte    $83, $82, $83, $82  ; . X . . ; track 0.5
.byte    $83, $82, $83, $84  ; . X X . ; track 0.75 ; 82/84 swapped so we can use 84 as a sentinel to break out of push/pull
.byte    $85, $85, $85, $85  ; . . X . ; track 1
.byte    $87, $87, $87, $87  ; . . X X ; track 1.25
.byte    $84, $84, $84, $84  ; . . . X ; track 1.5 ; ok to use the 84 sentinel because we're not in the push/pull state
.byte    $81, $81, $81, $81  ; X . . X ; track 1.75

make_step_table:

    LDX #$00
@0:
    TXA
    AND #$1F
    TAY
    LDA STEP_TABLE1_DATA,Y
    CLC
    ADC #(SLOTn0)
    STA STEP_TABLE1,X

    LDA STEP_TABLE2_DATA,Y
    CLC
    ADC #(SLOTn0)
    STA STEP_TABLE2,X
    INX
    BNE @0

; PHASE1STATE_TABLE == $83 if STEP_TABLE2_DATA == $83+SLOTn0 else $82
;
; here we don't pre-index by SLOTn0 because we need to do indirect addressing anyway at the point of use
; TODO: could avoid pre-indexing by SLOTn0 and self-modify the base address to be $C0n0
make_phase1_state_table:
    LDX #$00
@0:
    LDA #<PHASE1ON
    EOR STEP_TABLE2,X
    BNE @1
    ; Phase 1 is on
    LDA #<(PHASE1ON-SLOTn0)
    BNE @2
@1:
    ; Phase 1 is off
    LDA #<(PHASE1OFF-SLOTn0)
@2:
    STA PHASE1STATE_TABLE,X

    INX
    BNE @0

make_data:
    ; zero out data buffer
    LDX #$00
    LDA #$9F ; 0b10011111
@0:
    STA data,X
    INX
    BNE @0

    LDA #$FF
    STA data+1
    STA data+41
    STA data+80

seek_track0:
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

    ; phase 1 is off to begin with
    LDA #>(PHASE1OFF-SLOTn0)
    STA PHASE1STATE+1
    LDA #<(PHASE1OFF-SLOTn0)
    STA PHASE1STATE

    LDA #$00
    STA loopctr

    ;JMP prepare_read

prepare_write:
    ; allow time to settle
    ; TODO: excessive
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

    ; write-protect sense
    LDY #$60
    LDA LOADBASE,Y
    LDA READBASE,Y
    BPL @noerror
    STA MOTOROFF
    BRK

@noerror:
    ; start writing 40-cycle FF sync bytes
    LDA #$FF
    STA WRITEBASE,Y 
    CMP SHIFTBASE,Y

    ; XXX use fewer bytes
    STA ZPDUMMY
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

    ; XXX write fewer
    LDX #$FF ; number of sync bytes to write
    STA LOADBASE,Y 
    CMP SHIFTBASE,Y

    ; 40 - 9 = 31 cycles
    ; XXX use fewer bytes
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

    ; make sure last sync byte is a FF40 too
    NOP
    NOP
    NOP
    NOP
    NOP

    ; write header
    LDA #$D5
    JSR write_nibble9 ; 6 + 9 + (STA/CMP) + 6
    LDA #$AA ; 2
    JSR write_nibble9 ; 6 + 9 + (STA/CMP) + 6

    ; pad to 32 cycles until the next disk STA/CMP in disk_write_loop
    ; STA ZPDUMMY
    NOP

    ; inner loop counter
    LDX #LOOP_INNER

    STA ZPDUMMY ; 3

disk_write_loop:
    ; need to turn off phase 1 around all writes because the Disk II hardware suppresses writes if it is enabled
    ; We disable it unconditionally here and enable it conditionally later on, which saves some cycles
    LDY #$60 ; 2 XXX try to keep invariant
    ; LDA PHASE1OFF ; 4
    NOP
    NOP
    
    LDA data,X ; 4 byte to write

    ; write the data
    STA LOADBASE,Y ; 5
    CMP SHIFTBASE,Y ; 4

    ; reassert the current phase 1 state
    ;
    ; If phase 1 is on it will stop shifting out bits from now until we disable it again.
    ; This is a trade-off between moving the head and how many (and which) bits we can successfully write out.
    ; We delay this as much as possible to minimize the data loss.
    ; LDA (PHASE1STATE),Y ; 5
    STA ZPDUMMY
    NOP
        
    STA ZPDUMMY ; 3
    DEX ; 2
    BNE disk_write_loop ; 2/3
    ; falls through when it is time to step the head

write_step_head:
    INC loopctr ; 5
    NOP

    LDY #$60
    LDA #$FF ; 2
    STA LOADBASE,Y ; 5
    CMP SHIFTBASE,Y ; 4

    LDY loopctr ; 3
    LDX STEP_TABLE1,Y ; 4
    LDA $C000,X ; 4 toggle next phase switch
    LDX STEP_TABLE2,Y ; 4
    LDA $C000,X ; 4 toggle next phase switch

    LDX #$60
    LDA #$FF ; 2
    STA LOADBASE,X ; 5
    CMP SHIFTBASE,X ; 4

    LDX PHASE1STATE_TABLE,Y ; 4
    STX a:PHASE1STATE ; 4
    
    ; 2 reset inner loop counter1
    LDX #LOOP_INNER-1 ; we wrote an extra nibble
    BNE disk_write_loop ; 3 always

write_nibble9:
    CLC ; 2
    PHA ; 3
    PLA ; 4
    STA LOADBASE,Y ; 5
    ORA SHIFTBASE,Y ; 4
    RTS ; 6

prepare_read:
    LDA READ

    LDY #$60 ; XXX
    ; read 5 sync bytes
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
@0:
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

    ; now we have just missed the trailing edge of the (~8 cycle) valid read window by 1 cycle,
    ; so do a 27-cycle read this time to jump into the middle of the window and hopefully stay
    ; mostly centered there.  Drive speed is not constant though so there is still some drift.
    NOP
    NOP
    NOP
    NOP
    NOP

    ; inner loop counter
    LDX #LOOP_INNER

; 31 cycles in the common case

; need 3 variants
; 1. straight playback
; 2. push onto stack, previous to phase 1 on
; 3. pull from stack, during phase 1

disk_read_loop_nopush:
    BIT SHIFT ; 4
    ; should normally fall through, will occasionally loop once when
    ; we have slipped a cycle and the nibble is not ready after 31
    ; cycles
    BPL disk_read_loop_nopush ; 2/3

    ; keep same timing padding in all 3 variants
    NOP
    NOP
    STA ZPDUMMY

disk_read_loop_nopush_tail:
    BVC @notick ; 2/3
    STA $C030 ; 4
    BVS @next ; 3 always

@notick:
    ; 3+6 = 9
    NOP
    NOP
    NOP
@next:
    NOP
    NOP

    DEX ; 2
    BNE disk_read_loop_nopush ; 2/3

read_step_head_nopush:
    ; falls through when it's time to step the head
    ; 30 cycles so far, need 32 to get back on 31-cycle cadence

    INC loopctr ; 5
    LDX a:loopctr ; 4
    LDY STEP_TABLE1,X ; 4
    LDA $C000,Y ; 4 toggle next phase switch

    LDY STEP_TABLE2,X ; 4
    LDA $C000,Y ; 4 toggle next phase switch

    ; 2 reset inner loop counter1
    LDX #LOOP_INNER

    ; if STEP_TABLE2 == 89 then we are entering the last write sequence
    ; prior to enabling phase 1, so we need to transition to pushing stack values
    CPY #$89+SLOTn0
    BNE disk_read_loop_nopush ; 3 always
    ; falls through if we're entering the last write sequence prior to enabling phase 1

    ; 31 cycles when falling through

disk_read_loop_push:
    BIT SHIFT ; 4
    ; should normally fall through, will occasionally loop once when
    ; we have slipped a cycle and the nibble is not ready after 31
    ; cycles
    BPL disk_read_loop_push ; 2/3

    ; keep same timing padding in all 3 variants
    PHA
    STA ZPDUMMY

    BVC @notick ; 2/3
    STA $C030 ; 4
    BVS @next ; 3 always

@notick:
    ; 3+6 = 9
    NOP
    NOP
    NOP
@next:
    NOP
    NOP

    DEX ; 2
    BNE disk_read_loop_push ; 2/3

read_step_head_push:
    ; falls through when it's time to step the head
    ; 30 cycles so far, need 32 to get back on 31-cycle cadence

    STA ZPDUMMY

    INC loopctr ; 5
    LDX loopctr ; 3
    LDY STEP_TABLE1,X ; 4
    LDA $C000,Y ; 4 toggle next phase switch

    LDY STEP_TABLE2,X ; 4
    LDA $C000,Y ; 4 toggle next phase switch

    ; 2 reset inner loop counter1
    LDX #LOOP_INNER

    STA ZPDUMMY
    ; fall through to disk_read_loop_pull since phase 1 will be on and we couldn't
    ; write anything sensible

disk_read_loop_pull:
    BIT SHIFT ; 4
    ; should normally fall through, will occasionally loop once when
    ; we have slipped a cycle and the nibble is not ready after 31
    ; cycles
    BPL disk_read_loop_pull ; 2/3

    ; keep same timing padding in all 3 variants
    PLA
    ROL
    ROL
    
    BCC @notick ; 2/3
    STA $C030 ; 4
    BCS @next ; 3 always

@notick:
    ; 3+6 = 9
    NOP
    NOP
    NOP
@next:
    NOP
    NOP

    DEX ; 2
    BNE disk_read_loop_pull ; 2/3

read_step_head_pull:
    ; falls through when it's time to step the head
    ; 30 cycles so far, need 32 to get back on 31-cycle cadence

    INC loopctr ; 5
    LDX loopctr ; 3
    LDY STEP_TABLE1,X ; 4
    LDA $C000,Y ; 4 toggle next phase switch

    LDY STEP_TABLE2,X ; 4
    LDA $C000,Y ; 4 toggle next phase switch

    ; 2 reset inner loop counter1
    LDX #LOOP_INNER

    ; are we about to transition away from a push/pull phase 1 cycle?
    CPY #$84+SLOTn0
    BNE disk_read_loop_push ; 32
    ; falls through

; same header as disk_read_loop_nopush so we can fall through and then jump back at a convenient point
disk_read_loop_nopush2:
    BIT SHIFT ; 4
    ; should normally fall through, will occasionally loop once when
    ; we have slipped a cycle and the nibble is not ready after 31
    ; cycles
    BPL disk_read_loop_nopush2 ; 2/3

    ; keep same timing padding in all 3 variants
    NOP
    NOP
    JMP disk_read_loop_nopush_tail

done:
    STA MOTOROFF
    BRK

.endproc