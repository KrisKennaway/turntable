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

; Sequence of soft switches needed to move the head between tracks.
; TODO: use PHASE1OFF_BASE etc
; TODO: constants for sentinels
STEP_TABLE1_DATA:
.byte    $86, $86, $86, $86  ; X . . . ; track 0
.byte    $81, $80, $81, $80  ; X X . . ; track 0.25
.byte    $80, $80, $80, $80  ; . X . . ; track 0.5
.byte    $85, $84, $85, $82  ; . X X . ; track 0.75 ; 82/84 swapped so we can use 84 as a sentinel to break out of push/pull
.byte    $82, $82, $82, $82  ; . . X . ; track 1
.byte    $87, $87, $87, $87  ; . . X X ; track 1.25
.byte    $84, $84, $84, $84  ; . . . X ; track 1.5
.byte    $81, $81, $81, $81  ; X . . X ; track 1.75

STEP_TABLE2_DATA:
; In most cases we only need to flip one phase switch in order to move between tracks, so these
; values are the same as STEP_TABLE1_DATA.
;
; The only reason we need a second table is to handle the fact that the Disk II disables writes
; when phase 1 is active (i.e. tracks 0.25..0.75 mod 2).  To work around this, when we are
; working in this region we can only enable phase 1 for one sector at a time.  During the
; previous sector we prepare by pushing data onto the stack, and then pulling from it during
; the phase 1-enabled sector.
;
; To signal this we need two sentinel values:
; - $89 (=MOTORON) is used to indicate that the next sector begins an alternating phase 1 on/off
;   region
; - $84 indicates that we are leaving this region
; - we also make sure all of the $83 (=PHASE1_ON) are in this table instead of table 1 so we can
;   filter for them in make_phase1_state_table below

.byte    $86, $86, $86, $89  ; X . . . ; track 0 ; $89 is a sentinel to enter the push/pull mode when phase 1 is active
.byte    $83, $82, $83, $82  ; X X . . ; track 0.25
.byte    $83, $82, $83, $82  ; . X . . ; track 0.5
.byte    $83, $82, $83, $84  ; . X X . ; track 0.75 ; 82/84 swapped so we can use 84 as a sentinel to break out of push/pull
.byte    $85, $85, $85, $85  ; . . X . ; track 1
.byte    $87, $87, $87, $87  ; . . X X ; track 1.25
.byte    $84, $84, $84, $84  ; . . . X ; track 1.5 ; ok to use the 84 sentinel here because we're not in the push/pull state
.byte    $81, $81, $81, $81  ; X . . X ; track 1.75

; Populate a full page each from 8 copies of the above step tables.  That lets us easily iterate through without worrying about index bounds.
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

; Construct another table that has non-zero entries whenever phase 1 is active.  We need this when writing
; the disk image so we can write sync bytes leading in to the next sector.
; 
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

; Prepare for read or write
prepare:
    ; enable phase 0
    LDA PHASE0ON
    LDA PHASE1OFF
    LDA PHASE2OFF
    LDA PHASE3OFF

    LDA #$00
    STA loopctr

; wait for a keypress
; 1..3 plays the corresponding audio track
; W writes a new disk image (must be a blank disk)
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
    ; read data from disk and stage it into Slinky RAM so we can stream it back while writing
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

    ; write track header
    LDA #$D5
    JSR write_nibble9 ; 6 + 9 + (STA/CMP) + 6
    LDA #$AA ; 2
    JSR write_nibble9 ; 6 + 9 + (STA/CMP) + 6

    ; pad to 32 cycles until the next disk STA/CMP in disk_write_loop
    STA ZPDUMMY
    NOP

    ; Begin writing sectors
    ; inner loop counter
    LDX #LOOP_INNER
disk_write_loop:
    NOP
    NOP
    
    LDA SLINKY_DATA ; 4 byte to write

    ; write the data
    LDY #$60 ; 2
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

; We've written a sector, now actuate the next sector's phase switches, which may begin stepping the head.
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

    ; are we about to enable phase 1?
    LDA PHASE1STATE_TABLE,Y ; 4
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
    
    LDY #$60 ; 2
    ; TODO: no actual need to write here because phase 1 is on
    STA LOADBASE,Y ; 5
    CMP SHIFTBASE,Y ; 4

    ; 8
    NOP
    NOP
    NOP
    NOP
    
    DEX ; 2
    BNE disk_write_loop_phase1 ; 2/3

    ; write 7 more FF40 bytes so the Disk II can resync to the next sector once writes begin again
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
    ; TODO: roll this up into the previous loop
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

; Space-efficient time-wasting
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

; Construct tables that let us efficiently decode disk nibbles to pairs of bits in the 6-bit decoded
; representation, which lets us implement 3 audio tracks per disk
build_nibble_tables:
    ; clear tables since we're only writing sparse values
    LDX #$00
    LDA #$00
@0:
    STA DECODE_TRACK0,X
    STA DECODE_TRACK1,X
    STA DECODE_TRACK2,X
    INX
    BNE @0


    ; This part is copied from the Disk II ROM
    ;
    ; comments are from https://gswv.apple2.org.za/a2zine/GS.WorldView/Resources/DOS.3.3.ANATOMY/BOOT.PROCESS.txt
    ;
    ; We construct the table by sequentially
    ; incrementing (x) and testing it to see
    ; if it meets the folowing criteria of a
    ; disk byte:
    ;   (1) it must have at least one pair of
    ;       adjacent 1's in bits 0 to 6.
    ;   (2) it must not have more than one pair
    ;       of adjacent 0's in bits 0 to 6.
    ; (Note that we use the x-value to represent
    ; only the lower seven bits of a disk byte
    ; because all disk bytes are negative.)
    LDY #$00
    LDX #$3 ; Starting value for candidate valid nibble
@build_table:
    STX savex
    ; Transfer (x) to (a) and test to see if it
    ; meets the following disk byte criteria:
    ;  (1) has at least one pair of adjacent 1's
    ;      in bits 0 to 6.
    ;  (2) has no more than one pair of adjacent
    ;      0's in bits 0 to 6.

    ; Test for adjacent 1's.
    ;
    ; Note:  by comparing a shifted version of
    ; the seed (in accumulator) with the original
    ; version of the seed (in BT0SCRTH, $3C) we are
    ; actually testing adjacent bits as shown below:
    ;        Shifted:  b6   b5   b4   b3   b2   b1  b0   0
    ;        Orignal:  b7   b6   b5   b4   b3   b2  b1  b0
    ;                  -----------------------------------
    ;        Testing: b6,7 b5,6 b4,5 b3,4 b2,3 b1,2 b0,1 -
    TXA
    ASL
    BIT savex ; Conditions the z-flag of the status.
              ; (If any bits match, z-flag=1.)
    BEQ @next_x  ; Branch if value was illegal.
                 ; Illegal value = z-flag=1 = no match = no
                 ; adjacent 1's.
    ORA savex ; Merge shifted version of seed with orig.
    EOR #$FF  ;Take 1's compliment of shifted version to
              ;swap 1's for 0's and 0's for 1's.
    AND #%01111110 ; Throw away the hi and least significant
                   ; bits so will be testing:
                   ;    b5,6  b4,5  b3,4  b2,3 b1,2 b0,1.
@test_carry:
    BCS @next_x ; Always fall through on very first entry.
                ; If branch is taken, got illegal value
                ; because more than 1 pr of adjacent 0's.
    LSR         ; Shift a bit into the carry (if carry set
                ; have at least 1 pr of adjacent 0's).
    BNE @test_carry ; Take branch when remaining byte is not
                    ; zero.  Got at least 1 pr of adjacent 0's.
                    ; Go test carry to see if another pair has
                    ; already been detected.
    TYA ; Store the counter that corresponds to a

    ; Now shift A to the 6-bit value 0..3F that the nibble
    ; will decode to
    SEC
    SBC #$1F
    STA bit6_value

    ; save a copy so we can restore it for the next loop iteration
    STX savex

    ; add #$80 to X to match the nibble value we'll see from disk
    ; X contains the disk nibble ($96..$FF with sparse values)
    TXA
    EOR #$80
    TAX

    ; Now isolate neighbouring pairs of bits in the 6-bit decoded value,
    ; and shift them into bits 7 and 6.  We store these as entries in a
    ; table indexed by disk nibble value, so we can efficiently test them
    ; during disk reads by checking 6502 status flags (N and V)

    LDA bit6_value
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

; Begin audio playback
prepare_read:
    LDA READ

    LDY #$60
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
    LDA SHIFTBASE,Y
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

; We now enter a state machine reading sectors.  We need need 3 variants
; of sector reading:
;
; 1. straight playback
; 2. push onto stack, previous to a sector with phase 1 on
; 3. pull from stack, during a sector with phase 1 on

; 24 cycles in common case
disk_read_loop_nopush:
    LDY SHIFT ; 4
    ; should normally fall through, will occasionally loop but should only be once
    BPL disk_read_loop_nopush ; 2/3

decode_track_ref0:
    LDA DECODE_TRACK0,Y
    ;STA ZPDUMMY

    ; bit 7 signals whether we should toggle speaker
    BPL @notick ; 2/3
    STA $C030 ; 4
    BMI @next

@notick:
    NOP
    NOP
    NOP

@next:
    ; NOP
    
    CPY #$AA ; end of sector marker
    BNE disk_read_loop_nopush ; 2/3
    ; falls through when it's time to step the head

; we've read the sector, now flip soft switches to potentially begin stepping the head
read_step_head_nopush:
    ; toggle next phase switches
    INC loopctr ; 5
    LDX loopctr ; 3
    LDY STEP_TABLE1,X ; 4
    LDA $C000,Y ; 4
    LDY STEP_TABLE2,X ; 4
    LDA $C000,Y ; 4

    ; if STEP_TABLE2 == 89 then we are entering the last write sequence
    ; prior to enabling phase 1, so we need to transition to pushing stack values
    CPY #$89+SLOTn0
    BNE disk_read_loop_nopush ; 3
    ; falls through if we're entering the last write sequence prior to enabling phase 1

; 29 cycles in common case
disk_read_loop_push:
    LDY SHIFT ; 4
    ; should normally fall through, will occasionally loop once
    BPL disk_read_loop_push ; 2/3

decode_track_ref1:
    LDA DECODE_TRACK0,Y
    PHA ; also push data to the stack so we can use it during the next sector when phase 1 is active and no data is available on disk.

    BPL @notick ; 2/3
    STA $C030 ; 4
    BMI @next ; 3 always

@notick:
    NOP
    NOP
    NOP

@next:
    INX ; 2 count how many we are pushing

    CPY #$AA ; end of sector marker
    BNE disk_read_loop_push ; 2/3
    ; falls through when it's time to step the head

read_step_head_push:
    TXA
    PHA ; store how many bytes we pushed

    ; toggle next phase switches
    INC loopctr ; 5
    LDX loopctr ; 3
    LDY STEP_TABLE1,X ; 4
    LDA $C000,Y ; 4 

    LDY STEP_TABLE2,X ; 4
    LDA $C000,Y ;

    ; retrieve how many bytes we pushed
    PLA
    TAX

    ; fall through to disk_read_loop_pull since phase 1 will be on and we couldn't
    ; write anything sensible

; 28 cycles instead of 32 - so we know we'll finish ahead of schedule and haven't
; skipped past the sync bytes
disk_read_loop_pull:
    ; we have some free cycles, so check for a keypress to terminate playback
    BIT $C000
    BMI restart1

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
; We use D5 AB as the sector header
@startsync:
    LDA SHIFT ; TODO: not indexing by SLOTn0
    BPL @startsync
@tryd5:
    EOR #$D5
    BNE @startsync
@tryaa:
    LDA SHIFT ; 4
    BPL @tryaa ; 2/3
    CMP #$AB ; 2
    BNE @tryd5 ; 2/3

    ; TODO; need to add one more padding byte at start of sector?

read_step_head_pull:
    ;  toggle next phase switches
    INC loopctr ; 5
    LDX loopctr ; 3
    LDY STEP_TABLE1,X ; 4
    LDA $C000,Y ; 4
    LDY STEP_TABLE2,X ; 4
    LDA $C000,Y ; 4

    ; are we about to transition away from a push/pull phase 1 cycle?
    CPY #$84+SLOTn0
    ; nope
    BNE disk_read_loop_push ; 3

    ; We're done with phase 1, go back to regular sector reads
    JMP disk_read_loop_nopush

done:
    STA MOTOROFF
    BRK

restart1:
    JMP seek_track0

; Read audio data from ProDOS file and stage it into the slinky memory
; so we can stream it back when writing the audio track.
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

    ; TODO: not careful about EOF
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

open_cmdlist:
    .byte $03 ; param_count
    .addr pathname
    .addr IO_BUF
open_refnum:
    .byte 00 ; ref_num

pathname:
    .byte 10
    .asciiz "SOUND.DATA" ; TODO: unneeded trailing 0

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