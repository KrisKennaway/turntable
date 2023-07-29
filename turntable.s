; constants
SLOTn0 = $60 ; assumes slot 6

; how many nibbles to write before stepping head
LOOP_INNER = 254

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
loopctr = $00
ZPDUMMY = $01
savex = $03
bit6_value = $04

; internal buffers
STEP_TABLE1 = $4000
STEP_TABLE2 = $4100
PHASE1STATE_TABLE = $4200
DECODE_TRACK0 = $4300 ; maps bits 1, 0 of decoded nibble value to bits 7, 6
DECODE_TRACK1 = $4400 ; maps bits 3, 2 of decoded nibble value to bits 7, 6
DECODE_TRACK2 = $4500 ; maps bits 5, 4 of decoded nibble value to bits 7, 6
IO_BUF = $4600 ; 1K ProDOS I/O buffer
DATA_BUF = $4a00

HOME = $FC58
WAIT = $FCA8
BELL = $FF3A

MLI = $BF00
MLI_OPEN = $C8
MLI_READ = $CA

SLINKY_SLOTn0 = $40
SLINKY_ADDRL = $C080+SLINKY_SLOTn0
SLINKY_ADDRM = $C081+SLINKY_SLOTn0
SLINKY_ADDRH = $C082+SLINKY_SLOTn0
SLINKY_DATA = $C083+SLINKY_SLOTn0


.proc main
    JSR HOME
    CLC
    LDX #$c
@0:
    LDA banner,X
    ADC #$80
    STA $535,X
    DEX
    BPL @0

@done:
    JMP make_step_table

; STEP_TABLE: (entries have SLOTn0 added)

STEP_TABLE1_DATA:
; XXX use PHASE1OFF_BASE etc
; XXX constants for sentinels
;.byte    $86, $86, $86, $86  ; X . . . ; track 0
.byte    $86, $86, $86, $86  ; X . . . ; track 0

;.byte    $81, $80, $81, $80  ; X X . . ; track 0.25
.byte    $81, $80, $81, $80  ; X X . . ; track 0.25

;.byte    $80, $80, $80, $80  ; . X . . ; track 0.5
.byte    $80, $80, $80, $80  ; . X . . ; track 0.5

;.byte    $85, $84, $85, $84  ; . X X . ; track 0.75
.byte    $85, $84, $85, $82  ; . X X . ; track 0.75 XXX 82/84 swapped so we can use 84 as a sentinel

;.byte    $82, $82, $82, $82  ; . . X . ; track 1
.byte    $82, $82, $82, $82  ; . . X . ; track 1

;.byte    $87, $87, $87, $87  ; . . X X ; track 1.25
.byte    $87, $87, $87, $87  ; . . X X ; track 1.25

;.byte    $84, $84, $84, $84  ; . . . X ; track 1.5
.byte    $84, $84, $84, $84  ; . . . X ; track 1.5

;.byte    $81, $81, $81, $81  ; X . . X ; track 1.75
.byte    $81, $81, $81, $81  ; X . . X ; track 1.75

STEP_TABLE2_DATA:
; values are the same as STEP_TABLE1_DATA except when:
; - the next batch will turn phase 1 on ($89 = MOTORON is used as a sentinel for this)
; - we need to turn phase 1 on and off on alternating batches,
;   which may flipping two different switches
; - XXX 84 is also used as a sentinel value to break out of push/pull
; - we make sure all of the 83 are in this table so we can filter for them in make_phase1_state_table

;.byte    $86, $86, $86, $86  ; X . . . ; track 0 ;
.byte    $86, $86, $86, $89  ; X . . . ; track 0 ; 89 is a sentinel to enter the push/pull mode when phase 1 is active

;.byte    $83, $82, $83, $82  ; X X . . ; track 0.25
.byte    $83, $82, $83, $82  ; X X . . ; track 0.25

;.byte    $83, $82, $83, $82  ; . X . . ; track 0.5
.byte    $83, $82, $83, $82  ; . X . . ; track 0.5

;.byte    $83, $82, $83, $82  ; . X X . ; track 0.75 ;
.byte    $83, $82, $83, $84  ; . X X . ; track 0.75 ; 82/84 swapped so we can use 84 as a sentinel to break out of push/pull

;.byte    $85, $85, $85, $85  ; . . X . ; track 1
.byte    $85, $85, $85, $85  ; . . X . ; track 1

;.byte    $87, $87, $87, $87  ; . . X X ; track 1.25
.byte    $87, $87, $87, $87  ; . . X X ; track 1.25

;.byte    $84, $84, $84, $84  ; . . . X ; track 1.5 ; ok to use the 84 sentinel because we're not in the push/pull state
.byte    $84, $84, $84, $84  ; . . . X ; track 1.5 ; ok to use the 84 sentinel because we're not in the push/pull state

;.byte    $81, $81, $81, $81  ; X . . X ; track 1.75
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
    LDA #$FF
    BNE @2
@1:
    ; Phase 1 is off
    LDA #$0
@2:
    STA PHASE1STATE_TABLE,X

    INX
    BNE @0
    
    JSR build_nibble_tables

    ; spin up disk
    LDA DISK1SELECT
    LDA MOTORON

; Step to track 0
seek_track0:
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

    LDA #$00
    STA loopctr

@wait_key:
    LDA $C000
    BPL @wait_key
    BIT $C010

    ; check 1..3
    CMP #$b1 ; '1'
    BCC @check_write
    CMP #$b4 ; '4'
    BCS @check_write

    ; patch up track decode table references
    SEC
    SBC #($b1 - >DECODE_TRACK0)
    STA decode_track_ref0+2
    STA decode_track_ref1+2

    JMP prepare_read
@check_write:
    CMP #$D7 ; 'W'
    BEQ prepare_write

    ; unknown command
    JSR BELL
    JMP @wait_key

prepare_write:
    JSR load_slinky

    LDA #$00
    STA SLINKY_ADDRL
    STA SLINKY_ADDRM
    STA SLINKY_ADDRH

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

    ; 29 cycles
    JSR wait26
    STA ZPDUMMY

    ; XXX write fewer
    LDX #$FF ; number of sync bytes to write
    STA LOADBASE,Y 
    CMP SHIFTBASE,Y

    ; 40 - 9 = 31 cycles
    ; 5
    STA ZPDUMMY
    NOP
@0:
    ; 26 cycles
    JSR wait26

    STA LOADBASE,Y
    CMP SHIFTBASE,Y
    DEX
    BNE @0

    ; make sure last sync byte is a FF40 too
    ; 10
    STA ZPDUMMY
    PHA
    PLA

    ; write header
    LDA #$D5
    JSR write_nibble9 ; 6 + 9 + (STA/CMP) + 6
    LDA #$AA ; 2
    JSR write_nibble9 ; 6 + 9 + (STA/CMP) + 6

    ; pad to 32 cycles until the next disk STA/CMP in disk_write_loop
    STA ZPDUMMY
    NOP

    ; inner loop counter
    LDX #LOOP_INNER
disk_write_loop:
    NOP
    NOP
    
    LDA SLINKY_DATA ; 4 byte to write

    ; write the data
    LDY #$60 ; 2 XXX try to keep invariant
    STA LOADBASE,Y ; 5
    CMP SHIFTBASE,Y ; 4

    ; 8
    NOP
    NOP
    NOP
    NOP
    
    DEX ; 2
    BNE disk_write_loop ; 2/3
    ; falls through when it is time to step the head

    ; 9
    NOP
    PHA
    PLA

    LDA #$FF
    STA LOADBASE,Y ; 5
    CMP SHIFTBASE,Y ; 4

    ; 12
    NOP
    STA ZPDUMMY
    PHA
    PLA

write_step_head:
    INC loopctr ; 5
    NOP

    LDY #$60
    LDA #$D5 ; 2
    STA LOADBASE,Y ; 5
    CMP SHIFTBASE,Y ; 4

    LDY loopctr ; 3
    LDX STEP_TABLE1,Y ; 4
    LDA $C000,X ; 4 toggle next phase switch
    LDX STEP_TABLE2,Y ; 4
    LDA $C000,X ; 4 toggle next phase switch

    LDX #$60
    LDA #$AB ; 2
    STA LOADBASE,X ; 5
    CMP SHIFTBASE,X ; 4

    LDA PHASE1STATE_TABLE,Y ; 4 ; XXX
    BNE write_prepare_phase1
    NOP
    
    ; 2 reset inner loop counter1
    LDX #LOOP_INNER 
    BNE disk_write_loop ; 3 always

write_prepare_phase1:
    LDX #LOOP_INNER - 10 ; we're going to write 8 FF40 sync bytes which takes the same as 10 32-cycle writes
    NOP
    NOP

disk_write_loop_phase1:
    NOP
    NOP
    NOP
    ;BIT $C000
    ;BMI restart
    ;8
    NOP
    
    ; write the data
    LDY #$60 ; 2 XXX try to keep invariant
    ; XXX no actual need to write here because phase 1 is on
    STA LOADBASE,Y ; 5
    CMP SHIFTBASE,Y ; 4

    ; 8
    NOP
    NOP
    NOP
    NOP
    
    DEX ; 2
    BNE disk_write_loop_phase1 ; 2/3

    ; write 7 more FF40 bytes so the next write phase is synced
    LDX #$7
write_ff40:
    STA ZPDUMMY ; 3
    STA PHASE1OFF ; could do it outside the loop too
    LDA #$FF
    STA LOADBASE,Y ; 5
    CMP SHIFTBASE,Y ; 4

    ; 17
    JSR wait12
    STA ZPDUMMY
    NOP

    DEX
    BNE write_ff40

    ; 10
    STA ZPDUMMY
    PHA
    PLA

    ; one more FF40
    ; XXX roll this up
    STA LOADBASE,Y ; 5
    CMP SHIFTBASE,Y ; 4

    ;17
    JSR wait12
    STA ZPDUMMY
    NOP

    JMP write_step_head

write_nibble9:
    CLC ; 2
    PHA ; 3
    PLA ; 4
    STA LOADBASE,Y ; 5
    ORA SHIFTBASE,Y ; 4
    RTS ; 6

wait26:
    PHA
    PLA
wait19:
    PHA
    PLA
wait12:
    RTS

restart:
    JMP seek_track0

build_nibble_tables:
    ; clear tables
    LDX #$00
    LDA #$00
@0:
    STA DECODE_TRACK0,X
    STA DECODE_TRACK1,X
    STA DECODE_TRACK2,X
    INX
    BNE @0

    ; this part is copied from the Disk II ROM
    ; comments are from https://gswv.apple2.org.za/a2zine/GS.WorldView/Resources/DOS.3.3.ANATOMY/BOOT.PROCESS.txt
    ; XXX more comments
    LDX #$3
    STX savex
@build_table:
    STX savex ;Save potential index seed in the zero page.
    TXA
    ASL
    BIT savex ;Conditions the z-flag of the status.
                 ;(If any bits match, z-flag=1.)
    BEQ @next_x  ;Branch if value was illegal.
                      ;Illegal value = z-flag=1 = no match = no
                      ;adjacent 1's.
    ORA savex ;Merge shifted version of seed with orig.
    EOR #$FF     ;Take 1's compliment of shifted version to
                      ;swap 1's for 0's and 0's for 1's.
    AND #%01111110 ;Throw away the hi and least significant
                      ;bits so will be testing:
                      ;    b5,6  b4,5  b3,4  b2,3 b1,2 b0,1.
@test_carry:
    BCS @next_x  ;Always fall through on very first entry.
                      ;If branch is taken, got illegal value
                      ;because more than 1 pr of adjacent 0's.
    LSR          ;Shift a bit into the carry (if carry set
                      ;have at least 1 pr of adjacent 0's).
    BNE @test_carry ;Take branch when remaining byte is not
                      ;zero.  Got at least 1 pr of adjacent 0's.
                      ;Go test carry to see if another pair has
                      ;already been detected.
    TYA          ;Store the counter that corresponds to a

    SEC
    SBC #$1F
    STA bit6_value

    STX savex

    TXA
    EOR #$80
    TAX
    ; X contains the disk nibble ($96..$FF with sparse values)

    LDA bit6_value
    ; A contains the 6-bit logical value we are masking
    ASL
    ASL
    STA bit6_value
    AND #$C0 ; isolate bits 5 and 4 of decoded value
    STA DECODE_TRACK2,X

    LDA bit6_value
    ASL
    ASL
    STA bit6_value
    AND #$C0 ; isolate bits 3 and 2 of decoded value
    STA DECODE_TRACK1,X

    LDA bit6_value
    ASL
    ASL
    STA bit6_value
    AND #$C0 ; isolate bits 1 and 0 of decoded value
    STA DECODE_TRACK0,X

    LDX savex

    INY
@next_x:
    INX
    BPL @build_table ;Keep on trying to build table until (x)
                      ;increments to #$80.

    RTS

done2:
    STA MOTOROFF
    BRK

prepare_read:
    LDA READ

;    LDX #$FF
;    TXS
;    INX
;    LDA #$00
;@zero_stack:
;    STA $100,X
;    INX
;    BNE @zero_stack

    LDY #$60 ; XXX
    ; read 5 sync bytes
@loop:
    ldx #$5
@read:
    lda SHIFTBASE,y
    bpl @read
    cmp #$FF
    bne @loop
    dex
    bne @read

    ; sync to track start
@startsync:
    LDA SHIFTBASE,Y ; XXX
    BPL @startsync
@tryd5:
    EOR #$D5
    BNE @startsync

@tryaa:
    LDA SHIFTBASE,Y ; 4
    BPL @tryaa ; 2/3
    CMP #$AA ; 2
    BNE @tryd5 ; 2/3
    ; 14 cycles


    LDX #$00
; need 3 variants
; 1. straight playback
; 2. push onto stack, previous to phase 1 on
; 3. pull from stack, during phase 1 on

; 29 cycles in common case
disk_read_loop_nopush:
    LDY SHIFT ; 4
    ; should normally fall through, will occasionally loop once when
    ; we have slipped a cycle and the nibble is not ready after 31
    ; cycles
    BPL disk_read_loop_nopush ; 2/3

decode_track_ref0:
    LDA DECODE_TRACK0,Y
    ;STA ZPDUMMY

    BPL @notick ; 2/3
    STA $C030 ; 4
    ;BMI @next

@notick:
    ;NOP
    ;NOP
    ;NOP

@next:
    ; NOP
    
    INX ; 2 count how many we have read
    BEQ read_step_head_nopush ; in case we overflow

    CPY #$AA ; end of sector marker
    BNE disk_read_loop_nopush ; 2/3
    ; falls through when it's time to step the head

read_step_head_nopush:
    INC loopctr ; 5
    LDX loopctr ; 3

    LDY STEP_TABLE1,X ; 4
    LDA $C000,Y ; 4 toggle next phase switch
    LDY STEP_TABLE2,X ; 4
    LDA $C000,Y ; 4 toggle next phase switch

    LDX #$00

    ; if STEP_TABLE2 == 89 then we are entering the last write sequence
    ; prior to enabling phase 1, so we need to transition to pushing stack values
    CPY #$89+SLOTn0
    BNE disk_read_loop_nopush ; 3
    ; falls through if we're entering the last write sequence prior to enabling phase 1
    ; 31 cycles when falling through

; 29 cycles in common case
disk_read_loop_push:
    LDY SHIFT ; 4
    ; should normally fall through, will occasionally loop once when
    ; we have slipped a cycle and the nibble is not ready after 31
    ; cycles
    BPL disk_read_loop_push ; 2/3

decode_track_ref1:
    LDA DECODE_TRACK0,Y
    PHA

    BPL @notick ; 2/3
    STA $C030 ; 4
    ;BMI @next ; 3 always

@notick:
    ;NOP
    ;NOP
    ;NOP

@next:
    INX ; 2 count how many we are pushing
    BEQ read_step_head_push ; in case we overflow

    CPY #$AA ; end of sector marker
    BNE disk_read_loop_push ; 2/3
    ; falls through when it's time to step the head

read_step_head_push:
    TXA
    PHA ; store how many bytes we pushed

    INC loopctr ; 5
    LDX loopctr ; 3
    LDY STEP_TABLE1,X ; 4
    LDA $C000,Y ; 4 toggle next phase switch

    LDY STEP_TABLE2,X ; 4
    LDA $C000,Y ; 4 toggle next phase switch

    PLA
    TAX

    ; fall through to disk_read_loop_pull since phase 1 will be on and we couldn't
    ; write anything sensible

; 28 cycles instead of 32 - so we know we'll finish ahead of schedule
disk_read_loop_pull:
    BIT $C000
    BMI restart1

    ; keep same timing padding in all 3 variants
    PLP
    NOP 
    
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

    DEX ; 2
    BNE disk_read_loop_pull ; 2/3

; resync with sector lead
@startsync:
    LDA SHIFT ; XXX
    BPL @startsync
@tryd5:
    EOR #$D5
    BNE @startsync
@tryaa:
    LDA SHIFT ; 4
    BPL @tryaa ; 2/3
    CMP #$AB ; 2
    BNE @tryd5 ; 2/3

    ; XXX need to add one more padding byte at start of sector

    ; falls through when it's time to step the head

read_step_head_pull:
    INC loopctr ; 5
    LDX loopctr ; 3
    LDY STEP_TABLE1,X ; 4
    LDA $C000,Y ; 4 toggle next phase switch

    LDY STEP_TABLE2,X ; 4
    LDA $C000,Y ; 4 toggle next phase switch

    ; are we about to transition away from a push/pull phase 1 cycle?
    CPY #$84+SLOTn0
    BNE disk_read_loop_push ; 3

    LDX #$00

    JMP disk_read_loop_nopush

done:
    STA MOTOROFF
    BRK

restart1:
    JMP seek_track0

load_slinky:
    LDX #$00
    STX SLINKY_ADDRL
    STX SLINKY_ADDRM
    STX SLINKY_ADDRH

    JSR MLI
    .byte MLI_OPEN
    .addr open_cmdlist
    BNE @error

    LDA open_refnum
    STA read_refnum

@read_block:
    INC $400
    JSR MLI
    .byte MLI_READ
    .addr read_cmdlist
    CLC
    BEQ @store_data

    CMP #$4C  ; EOF
    BNE @error
    SEC ; signal EOF

    ; XXX not careful about EOF
@store_data:
    LDX #$00
@0:
    LDA DATA_BUF,X
    STA SLINKY_DATA
    INX
    BNE @0

    BCC @read_block

    RTS
@error:
    BRK

;@play:
;    LDA #$00
;    STA SLINKY_ADDRL
;    STA SLINKY_ADDRM
;    STA SLINKY_ADDRH
;
;playback:
;    LDA SLINKY_DATA
;    ROR
;
;    BCC @notick ; 2/3
;    STA $C030 ; 4
;    BCS @next ; 3 always
;
;@notick:
;    ; 3+6 = 9
;    NOP
;    NOP
;    NOP
;
;@next:
;    STA $401
;    ; end playback on keypress
;    BIT $C000
;    BMI @done
;    NOP
;    NOP
;    JMP playback
;
;@done:
;    BIT $C010
;    RTS

open_cmdlist:
    .byte $03 ; param_count
    .addr pathname
    .addr IO_BUF
open_refnum:
    .byte 00 ; ref_num

pathname:
    .byte 10
    .asciiz "SOUND.DATA" ; XXX unneeded trailing 0

read_cmdlist:
    .byte $04 ; param_count
read_refnum:
    .byte 00 ; ref_num
    .addr DATA_BUF
    .word $0100 ; request_count
    .word $0000 ; transfer_count

banner:
    .asciiz "( TurnT@ble )"

.endproc