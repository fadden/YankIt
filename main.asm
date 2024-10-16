;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; main.asm - startup/shutdown routines, command line argument processing
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

           MLOAD YankIt.macros


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
main       START
;
; Entry point into program; called by system loader.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
           using globals

           phx                            ;hi word of arg addr
           phy                            ;lo word

;           SetStone #"Here we go"         ;debug
           jsr   StartUp
           bcc   StartOK                  ;couldn't even start up!
;           SetStone #"StartUp failed"     ;debug
           brl   Fail
StartOK    anop
;           SetStone #"Started up"         ;debug
           ply
           plx
           phk
           plb                            ;set B to main program

           sty   <argv                    ;GS/OS set our dp
           stx   <argv+2

;           tdc                            ;debug
;           cmp   #directpage              ;debug
;           beq   dp_ok                    ;debug
;           writeln #"ERROR: DP is bad"    ;debug
;dp_ok      anop                           ;debug

; init some DP stuff
           stz   <abort

           jsr   CheckVersion
           bcc   vers_ok
           brl   Fail
vers_ok    anop

;           SetStone #"Processing args..." ;debug
           jsr   ProcessArgs
           bcc   goodargs
;           SetStone #"Usage"              ;debug
           jsr   Usage
           brl   Fail

goodargs   ANOP
;           lda   <byteworks               ;debug
;           beq   notbw                    ;debug
;           writeln #"byteworks"           ;debug
;notbw      anop                           ;debug

;           SetStone #"Valid args"         ;debug
           jsr   InitCRCTab
           lda   <mode
           and   #dNoSession
           bne   nobs
           jsr   BeginSession             ;GS/OS deferred write
nobs       jsr   OpenArchive
           bcc   openok
           brl   Fail
openok     anop
           jsr   OpenConsole              ;ignore errors for this one
           jsr   ProcessMHBlock           ;read & interpret master header
           bcs   CFail

loop       ANOP
           lda   <byteworks               ;don't try this on other shells
           beq   nostop
           StopGS pStop                   ;ignore error status
           lda   pStop+$02                ;check the result
           beq   nostop
           inc   <abort
           bra   loopdone                 ;use hit OA-., so quit
nostop     anop
           jsr   ProcessRecord            ;view it, extract it, whatever
           bcs   CFail
           lda   <abort
           bne   loopdone                 ;don't fail, but don't continue
           dec   <total_records           ;assume 0 < total_records < 65535
           bne   loop

loopdone   anop
           jsr   CloseFile                ;close open output file, if any
           bcs   CFail
           jsr   CloseConsole
           jsr   CloseArchive
           bcs   Fail

Done       ANOP
;           SetStone #"Shutting down"      ;debug
           lda   <mode
           bit   #dView
           bne   done2                    ;don't clutter view mode
           bit   #dVerbose
           beq   done2                    ;don't clutter non-verbose mode
           pea   donestr|-16
           pea   donestr
           _WriteCString
done2      anop
           lda   <mode
           and   #dNoSession
           bne   noes
           jsr   EndSession
noes       jsr   ShutDown
           lda   #$0000
           rtl                            ;should do QUIT here

CFail      ANOP                           ;fail, but close archive first
           jsr   CloseFile                ;close open output file, if any
           jsr   CloseConsole
           jsr   CloseArchive
           bcc   Fail                     ;that's a switch
           jsr   ToolError                ;print another message & fall thru

Fail       ANOP
;           SetStone #"Failure, shutting down" ;debug
           lda   <mode
           and   #dNoSession
           bne   enoes
           jsr   EndSession
enoes      jsr   ShutDown
           lda   #$ffff
           rtl

donestr    dc    C'Done.',H'0d0a00'
pStop      ANOP
           dc    I'1'                     ;pCount
           ds    2                        ;flag (result)
           END


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
globals    DATA
;
; Global direct page usage and global defined constants
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 
; Defined constants
;
dMinVersion equ  $0303                    ;minimum version of GS/OS

dView      equ   $0001                    ;view only
dExtract   equ   $0002                    ;extract files
dIntegrity equ   $0004                    ;integrity check - extract w/o write
dStdin     equ   $0100                    ;TRUE if archive is stdin
dVerbose   equ   $0200                    ;verbose output mode
dNoSession equ   $0400                    ;don't use sessions
dForce     equ   $0800                    ;force overwrite of existing files
dFile      equ   $0000                    ;rec_kind (is non-forked file)
dDisk      equ   $0001                    ;rec_kind (is a disk image)
dForked    equ   $0002                    ;rec_kind (is a forked file)
dOther     equ   $0003                    ;rec_kind (none of the above)

dPakBufSiz gequ  32768                    ;32K pack buffer (also in ShrinkMem)

dPercFreq  equ   4                        ;update % msg after every 4 writes

dMaxFargs  equ   32                       ;at most 32 files specified

;
; YankIt error codes
;
eNotArchive equ  0+$0080                  ;match on "NuFile" failed
eBadMHBlock equ  1+$0080                  ;corrupted master header (bad CRC)
eBadRHId   equ   2+$0080                  ;bad record (didn't find "NuFX")
eHugeRH    equ   3+$0080                  ;record header is too huge
eLotsaThreads equ 4+$0080                 ;too many threads in this record
eNoThreads equ   5+$0080                  ;no threads in this record
eBadRHBlock equ  6+$0080                  ;corrupted record header (bad CRC)
eEofHit    equ   7+$0080                  ;EOF hit early (but GS/OS says ok)
eStackBlown equ  8+$0080                  ;blew the stack in UndoLZW_X
eBadVersion equ  9+$0080                  ;not high enough version of GS/OS
eBadThread equ   10+$0080                 ;bad thread data (bad CRC)
eFileCLoop equ   11+$0080                 ;ended up in infinite loop

;
; direct page
;
; general usage / temp stuff
; (this could be better organized)
ptr        equ   $00                      ;(4b) general pointer
ptr2       equ   $04                      ;(4b) general pointer
tmp        equ   $08                      ;(2b) temporary storage
subtmp     equ   $0a                      ;(2b) minor subroutine temp storage
subtmp2    equ   $0c                      ;(2b) (same, must be contiguous)
crc        equ   $0e                      ;(2b) used during crc calculation
fargtmp    equ   $10                      ;(2b) used during farg match
tmpc       equ   $12                      ;(2b) used during farg match
fnloop     equ   $14                      ;(2b) used during filename handling
fnptr      equ   $16                      ;(2b) used during path creation
fnptr2     equ   $18                      ;(2b) used during path creation
fnptr3     equ   $1a                      ;(2b) used during path creation

; global vars
argv       equ   $20                      ;(4b) pointer to command line
mode       equ   $24                      ;(2b) one of dView, dExtr., dIntegr.
last_error equ   $26                      ;(2b) last error we encountered
file_posn  equ   $28                      ;(4b) archive file position
dotconsole equ   $2c                      ;(2b) device # of .CONSOLE
abort      equ   $2e                      ;(2b) if nonzero, abort whole thing
fargcount  equ   $30                      ;(2b) #of file args on command line
byteworks  equ   $32                      ;(2b) nonzero if this is APW or Orca

; archive vars
master_crc equ   $40                      ;(2b) MH master_crc
total_records equ $42                     ;(4b) MH total_records

; current record vars
total_threads equ $60                     ;(2b) RH total_threads
filename_sep equ $62                      ;(2b) RH file_sys_info (low byte)
skip_mode  equ   $64                      ;(2b) true-->skip remaining threads
rec_kind   equ   $66                      ;(2b) used by view mode
total_clen equ   $68                      ;(4b) total compressed length
data_unlen equ   $6c                      ;(4b) data fork uncompressed length
rsrc_unlen equ   $70                      ;(4b) rsrc fork uncompressed length
total_unlen equ  $74                      ;(4b) data_unlen+rsrc_unlen
rec_format equ   $78                      ;(2b) used by view mode
created    equ   $7a                      ;(2b) has file been created?
which      equ   $7c                      ;(2b) which fork?

; ShrinkIt LZW vars - globals
type2      equ   $80                      ;(2b) static boolean
escape_char equ  $82                      ;(2b) static onebyt
out_bytes  equ   $84                      ;(2b) static int
stack_ptr  equ   $86                      ;(2b) static int
entry      equ   $88                      ;(2b) static int
at_bit     equ   $8a                      ;(2b) static int
at_byte    equ   $8c                      ;(2b) static int
last_byte  equ   $8e                      ;(2b) static onebyt
ibuf       equ   $90                      ;(2b) static onebyt *
reset_fix  equ   $92                      ;(2b) boolean (LZW-II bug fix)
; main loop vars
blkCRC     equ   $a0                      ;(2b) twobyt
wrbuf      equ   $a2                      ;(2b) onebyt *
unlen      equ   $a4                      ;(2b) short
lzwflag    equ   $a6                      ;(2b) short
rleflag    equ   $a8                      ;(2b) short
complen    equ   $aa                      ;(2b) short
partial    equ   $ac                      ;(2b) unsigned int
toread     equ   $ae                      ;(2b) unsigned int
in_buf     equ   $b0                      ;(2b) unsigned int
space      equ   $b2                      ;(2b) unsigned int (was fourbyt tmp4)
thread_eof equ   $b4                      ;(4b) fourbyt
c_t_eof    equ   $b8                      ;(4b) fourbyt
thread_crc equ   $bc                      ;(2b) twobyt
old_stack  equ   $be                      ;(2b) used for error recovery
percount   equ   $c0                      ;(2b) countdown to next "xx% done"
othread_eof equ  $c2                      ;(4b) original thread_eof
; subroutine vars (GetCode, UndoLZW1, UndoLZW2, UndoRLE)
oldc       equ   $d0                      ;(2b) int
incode     equ   $d2                      ;(2b) int
finalc     equ   $d4                      ;(2b) int
;lptr       equ   $d6                      ;(2b) int
total      equ   $d8                      ;(2b) int
count      equ   $da                      ;(2b) ....
;foo2       equ   $dc                      ;(2b) ....
;num_bits   equ   $de                      ;(2b) int
old_bit    equ   $e0                      ;(2b) int
last_bit   equ   $e2                      ;(2b) int
value      equ   $e4                      ;(4b) long
mask       equ   $e8                      ;(2b) unsigned int (was long)

; debugging stuff
debug_rgn  equ   $f0                      ;(16b) debugging only
           END


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
StartUp    PRIVATE
;
; Calling conventions:
;   JSR
;   Returns with carry set on failure
;
; Starts up tools.  This is a cut-down version of a startup routine I
; use in other programs, and it has a lot of stuff that it doesn't
; really need...
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;           using ParmBlocks               ;DEBUG

           _TLStartUp                     ;start tool locator

           pea   $0000
           _MMStartUp                     ;start memory manager
           bcs   Crash
           pla
           sta   >UserID

; start up misc/math/text tools
           _MTStartUp
           _IMStartUp
           _TextStartUp

; initialize text masks (probably unnecessary)
           pea   $ffff                    ;AND mask
           pea   $0000                    ;OR mask
           _SetInGlobals
           pea   $ffff
           pea   $0000
           _SetOutGlobals
           pea   $ffff
           pea   $0000
           _SetErrGlobals

;;debug: force prefix to /ram5
;           _GetPrefixGS pGetPrefix        ;DEBUG
;           bcc   ok1
;           jsr   ToolError
;ok1        anop
;           _SetPrefixGS pDebugPrefix      ;DEBUG - all output to /ram5/
;           bcc   ok2
;           jsr   ToolError
;ok2        anop

           rts


; if something goes wrong
Crash      ANOP
           jsr   ToolError                ;num is in ACC
           sec
           rts

; hang onto this stuff
UserID     ENTRY
           ds    2
           END


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
ShutDown   PRIVATE
;
; Calling conventions:
;   JSR
;
; Shuts down tools.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;           using ParmBlocks               ;DEBUG
;           _SetPrefixGS pResetPrefix      ;DEBUG
;           bcc   ok
;           jsr   ToolError
;ok         anop

           _IMShutDown
           _TextShutDown
           _MTShutDown
           lda   >UserID
           pha
           _MMShutDown
           _TLShutDown
           rts
           END


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
ProcessArgs PRIVATE
;
; Calling conventions:
;   JSR
;   Returns with carry set if something was missing on the command line.
;
; Sets "mode" and "archivename" according to the command line arguments.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
           using globals
           using ParmBlocks               ;for shell stuff

           lda   #$0001                   ;init mode
           sta   <mode
           shortm

; first, see if this is a byteworks shell
           ldy   #$0000
           ldx   #$0008
           stz   <byteworks
bwloop     anop
           lda   [<argv],y
           cmp   bwid,y
           bne   notbw
           iny
           dex
           bne   bwloop
           inc   <byteworks               ;note this is 8-bit (no diff tho)
notbw      anop

; go get some args
           ldy   #$0008                   ;skip past "BYTEWRKS" or whatever
           jsr   skipwspc                 ;move forward to command invocation
           bcs   badargs
           jsr   skiptok                  ;move to end of token
           bcs   badargs
           jsr   skipwspc                 ;move to command letter (v/x/i)
           bcs   badargs

           lda   [<argv],y
           and   #$7f
           cmp   #$60
           blt   isupper
           sec
           sbc   #$20                     ;convert to upper
isupper    anop
           cmp   #"T"                     ;view (table of contents)?
           beq   getothers
           asl   <mode
           cmp   #"X"                     ;extract?
           beq   getothers
           asl   <mode
           cmp   #"I"                     ;integrity check?
           beq   getothers

badargs    anop
           jmp   Fail

; we weenie around the whitespace issue by assuming we hit whitespace
; when we find a character we don't recognize
getothers  anop
           iny
           lda   [<argv],y
           and   #$7f
           cmp   #$60
           blt   isupper2
           sec
           sbc   #$20                     ;convert to upper
isupper2   anop
           cmp   #"V"                     ;verbose mode
           bne   notv
           lda   <mode+1
           ora   #dVerbose|-8             ;(remember we're shortm)
           sta   <mode+1
           bra   getothers
notv       cmp   #"S"                     ;don't use sessions
           bne   nots
           lda   <mode+1
           ora   #dNoSession|-8
           sta   <mode+1
           bra   getothers
nots       cmp   #"F"                     ;overwrite existing files
           bne   notf
           lda   <mode+1
           ora   #dForce|-8
           sta   <mode+1
           bra   getothers

notf       anop                           ;back up to non-whitespace char
           dey                            ; and fall through to getfname

getfname   ANOP
           jsr   skiptok
           bcs   badargs
           jsr   skipwspc
           bcs   badargs
           longm                          ;copy "archivename" from here
           tya
           clc
           adc   <argv                    ;set ptr to src
           sta   <ptr
           lda   <argv+2
           sta   <ptr+2
           lda   #wfilename+2              ;set ptr2 to dst
           sta   <ptr2
           lda   #wfilename|-16
           sta   <ptr2+2

; compute the filename's length
           sty   <tmp
           shortm
           jsr   skiptok                  ;find first char past token
           longm
           tya                            ;new posn is in Y
           sec
           sbc   <tmp                     ;subtract addr of last token
           sta   wfilename                ;store it
           sty   <tmp                     ;save new Y
           tax                            ;#of bytes to copy
           lda   #$0000                   ;make it null-terminated
           sta   wfilename+2,x
           jsr   bcopy                    ;copy from ptr to ptr2

           ldy   <tmp
           lda   wfilename
           cmp   #$0001                   ;one char long?
           bne   notstdin
           dey
           shortm
           lda   [<argv],y                ;was it a dash?
           iny                            ;just in case we use this later on
           and   #$7f
           cmp   #"-"
           bne   notstdin
           longm
           lda   <mode
           ora   #dStdin
           sta   <mode
;           bra   cont
           brl   cont

notstdin   anop
           longm
; okay, now we get to play "expand the filename" with INIT_WILDCARD...
; We try to do this even if the shell isn't BYTEWRKS, in the hope that
; maybe they support it anyway to be compatible with Orca.
;           writestr #"wfilename: "
;           pea   wfilename|-16            ;debug
;           pea   wfilename+2              ;debug
;           _WriteCString                  ;debug
;           lda   wfilename                ;debug
;           jsl   PrintNum                 ;debug
           stz   archivename

           lda   wfilename                ;get length
           cmp   #$0100                   ;shell uses a length BYTE
           bge   toobig                   ;too big, skip it
           xba                            ;swap length bytes around
           sta   wfilename

; this should make it work under ECP-16 and ProSel-16 w/o causing errors
           lda   <byteworks
           beq   i_f2                     ;don't even try

           INIT_WILDCARD pInitWild
           bcs   initw_failed

           NEXT_WILDCARD pNextWild
;           bcs   initw_failed            ;no possible errors

           lda   archivename+1            ;get new length
           and   #$00ff                   ;strip hi byte (part of name)
           beq   i_f2                     ;wildcard didn't match... sigh

           lda   archivename
           xba                            ;swap them back
           sta   archivename
           bra   cont

initw_failed anop
           cmp   #$0001                   ;invalid call - this shell doesn't
           beq   i_f2                     ; support wildcard matching
           pha
           pea   iwfail|-16
           pea   iwfail
           _ErrWriteCString
           pla
           jsr   ToolError
i_f2       anop
; for whatever reason, we don't have something valid in "archivename".
; so, just bcopy wfilename to archivename.
           lda   wfilename                ;switch length bytes back
           xba
           sta   wfilename
toobig     anop
           lda   #wfilename
           sta   <ptr
           lda   #wfilename|-16
           sta   <ptr+2
           lda   #archivename
           sta   <ptr2
           lda   #archivename|-16
           sta   <ptr2+2
           ldx   wfilename
           inx                            ;+2 for length word
           inx
           jsr   bcopy
cont       anop
           ldx   archivename
           stz   archivename+2,x          ;null-terminate it

; now for more fun: grab the filenames given on the command line, if any
           stz   <fargcount
           ldy   <tmp

           shortm
           ldx   #$0000
fargloop   anop
           jsr   skipwspc                 ;skip intervening whitespace
           bcs   farg_done                ;hit nul again

           longm
           tya
           clc
           adc   <argv
           sta   fargv,x
           inx
           inx
           lda   <argv+2
           sta   fargv,x
           inx
           inx
           shortm
           jsr   skiptok
           lda   #$00
           sta   [<argv],y                ;null-terminate this one
           iny
           inc   <fargcount
           bra   fargloop

farg_done  anop
           longm

Done       ANOP
;           writestr #"--- archive name: " ;DEBUG
;           pea   archivename|-16
;           pea   archivename+2
;           _WriteCString
;           lda   archivename
;           jsl   PrintNum                 ;DEBUG
;           writestr #"--- fargcount: "    ;debug
;           lda   <fargcount               ;debug
;           jsl   PrintNum                 ;debug

           clc
           rts

Fail       ANOP
           longm
           sec
           rts

iwfail     dc    C'warning: wildcard match failed: ',H'00'
bwid       dc    C'BYTEWRKS'
           END


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
skipwspc   START
;
; Calling conventions:
;   JSR with the Y-reg indexed into the current argv position.  Expects
;     shortm, longx.
;   Returns with the Y-reg set to the next non-whitespace character.  Sets
;     the carry if it encounters a NUL (00) before finding anything else.
;
; Skip whitespace in the argument vector.  Whitespace is defined as spaces,
; tabs, carriage returns, and line feeds.  If the character pointed at is
; not a whitespace char, this returns immediately.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
           LONGA OFF
           using globals

           dey
loop       ANOP
           iny
           lda   [<argv],y
           and   #$7f
           cmp   #$20                     ;space?
           beq   loop
           cmp   #$09                     ;tab (ctrl-I)?
           beq   loop
           cmp   #$0a                     ;line feed (ctrl-J)?
           beq   loop
           cmp   #$0d                     ;carriage return (ctrl-M)?
           beq   loop
           cmp   #$00                     ;null (ctrl-@)?
           bne   gotsomething
           sec                            ;end of line
           rts

gotsomething ANOP
           clc
           rts
           LONGA ON
           END


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
skiptok    START
;
; Calling conventions:
;   JSR with the Y-reg indexed into the current argv position.  Expects
;     shortm, longx.
;   Returns with the Y-reg set to the start of the next whitespace.  Sets
;     the carry if it encounters a NUL (00) before finding anything else.
;
; Finds the end of the current "token" in the command line by skipping over
; all non-whitespace characters.  The Y-reg will be set to point at the
; first whitespace character following the token.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
           LONGA OFF
           using globals

loop       ANOP
           iny                            ;first char is unimportant
           lda   [<argv],y
           and   #$7f
           cmp   #$20                     ;space?
           beq   white
           cmp   #$09                     ;tab (ctrl-I)?
           beq   white
           cmp   #$0a                     ;line feed (ctrl-J)?
           beq   white
           cmp   #$0d                     ;carriage return (ctrl-M)?
           beq   white
           cmp   #$00                     ;null (ctrl-@)?
           bne   loop
           sec                            ;end of line
           rts

white      clc
           rts
           LONGA ON
           END


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
MatchName  START
;
; Calling conventions:
;   JSR
;   Returns with carry set if a match is found, clear otherwise.
;
; Determines if the contents of filename_buf match any of the entries in
; fargv.  If fargv is empty (fargcount==0) then the carry is always set
; on return.  Lower case characters are converted to upper, and the hi
; bit is stripped before the match is performed.
;
; DP USAGE:
;   subtmp, subtmp2
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
           using globals

           lda   <fargcount
           beq   match                    ;no specs, so always match
           sta   <fargtmp

loop       ANOP
           lda   <fargtmp
           beq   nomatch
           dec   A
           sta   <fargtmp
           asl   A
           asl   A
           tax

           lda   fargv,x
           sta   <subtmp
           lda   fargv+2,x
           sta   <subtmp+2                ;<subtmp2

           shortm
           ldy   #$0000
matchloop  anop
           lda   filename_buf+2,y
           and   #$7f
           cmp   #$60
           blt   isupper2
           sec
           sbc   #$20
isupper2   anop
           sta   <tmpc

           lda   [<subtmp],y
           and   #$7f                     ;strip hi bit
           cmp   #$60                     ;lower case?
           blt   isupper
           sec
           sbc   #$20
isupper    anop
           tax                            ;for later

           cmp   <tmpc                    ;did it match?
           bne   trynext                  ;no, try the next one
           cmp   #$00                     ;reached the end on both strings?
           beq   match                    ;yes, so this one matched

           txa
           and   filename_buf+2,y         ;reached the end on either string?
           beq   trynext
           iny
           bra   matchloop

trynext    ANOP
           longm
           bra   loop

match      ANOP
           longm
           sec
           rts

nomatch    ANOP
           longm
           clc
           rts
           END


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
Usage      PRIVATE
;
; Calling conventions:
;   JSR
;
; Prints usage information to stderr.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
           pea   usagestr|-16
           pea   usagestr
           _ErrWriteCString
           rts

usagestr   ANOP
           dc    C'YankIt v1.21   Copyright (C) 1992 by Andy McFadden'
           dc    C'   Freeware',H'0d0a'
           dc    H'0d0a'
           dc    C'Usage: YankIt t[vfs]|x[vfs]|i[vfs] archive.shk '
           dc    C'[file1 file2 ...]',H'0d0a'
           dc    C'    t - table of contents        v - verbose',H'0d0a'
           dc    C'    x - extract                  f - force overwrite'
           dc    H'0d0a'
           dc    C"    i - integrity check          s - don't use sessions"
           dc    H'0d0a'
           dc    H'00'

           END


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
ToolError  START
;
; Calling conventions:
;   JSR with error number in Acc.
;
; Handles toolbox errors (prints an error message).  I use numbers instead
; of symbolic constants for the error messages because I hate waiting
; for Orca to re-interpret the damn macro files every time.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
           using globals
;           tax                            ;debug
;           lda   #ToolError|-16           ;debug
;           jsl   PrintNum                 ;debug
;           pla                            ;yank return addr
;           jsl   PrintNum                 ;debug
;           pha                            ;debug
;           txa                            ;debug

           pha                            ;used by Int2Hex
           sta   <last_error

;           lda   <last_error
           cmp   #$100
           blt   gsos_err

           pea   terrstr|-16              ;Int2Hex(num, terrstr+12, 4)
           pea   terrstr+12
           pea   $0004                    ;output is 4 chars
           _Int2Hex

           pea   terrstr|-16              ;ErrWriteCString(terrstr)
           pea   terrstr
           _ErrWriteCString
           bra   verbose_msg

gsos_err   ANOP
           pea   gerrstr|-16
           pea   gerrstr+13
           pea   $0004
           _Int2Hex

           pea   gerrstr|-16
           pea   gerrstr
           _ErrWriteCString


verbose_msg ANOP
; look for some common ones
           lda   <last_error

           ldy   #$0000
loop       lda   code_tab,y
           beq   not_found
           cmp   <last_error
           beq   found
           iny
           iny
           bra   loop

found      anop
           lda   addr_tab,y
           pea   e0004|-16                ;doesn't matter which
           pha
           pea   dash|-16
           pea   dash
           _ErrWriteCString
           _ErrWriteCString

not_found  anop

Done       ANOP
           pea   crlf|-16
           pea   crlf
           _ErrWriteCString
;           brk   0                        ;debug
           lda   #$ffff                   ;put error result into acc
           sec
           rts

terrstr    dc    C'Tool Error $    ',H'00'
gerrstr    dc    C'GS/OS Error $    ',H'00'
crlf       dc    H'0d0a00'
dash       dc    C' - ',H'00'
code_tab   dc    I2'$0004,$0010,$002b,$0040,$0044,$0046,$004c,$004d'
           dc    I2'$004e,$0201,$0000'
addr_tab   dc    A2'e0004,e0010,e002b,e0040,e0044,e0046,e004c,e004d'
           dc    A2'e004e,e0201'
e0004      dc    C'bad param count (bug!)',H'00'
e0010      dc    C'file (device) not found',H'00'
e002b      dc    C'write protected',H'00'
e0040      dc    C'invalid pathname',H'00'
e0044      dc    C'path not found',H'00'
e0046      dc    C'file not found',H'00'
e004c      dc    C'reached EOF unexpectedly',H'00'
e004d      dc    C'position out of range (archive possibly corrupted)',H'00'
e004e      dc    C'file locked',H'00'
e0201      dc    C'unable to allocate memory',H'00'

           AGO .OLD_ERR

;           tax                            ;debug
;           lda   #ToolError|-16           ;debug
;           jsl   PrintNum                 ;debug
;           pla                            ;yank return addr
;           jsl   PrintNum                 ;debug
;           pha                            ;debug
;           txa                            ;debug

           pha                            ;used by Int2Hex
           sta   <last_error

           pea   $ffff                    ;AND mask
           pea   $0000                    ;OR mask
           _SetErrGlobals

           lda   <last_error
           cmp   #$100
           blt   gsos_err

           pea   terrstr|-16              ;Int2Hex(num, terrstr+12, 4)
           pea   terrstr+12
           pea   $0004                    ;output is 4 chars
           _Int2Hex

           pea   terrstr|-16              ;ErrWriteCString(terrstr)
           pea   terrstr
           _ErrWriteCString
           brl   Done

gsos_err   ANOP
           pea   gerrstr|-16
           pea   gerrstr+13
           pea   $0004
           _Int2Hex

           pea   gerrstr|-16
           pea   gerrstr
           _ErrWriteCString

; look for some common ones
           lda   <last_error

           cmp   #$0004                   ;invalidPcount
           bne   not04
           lda   #e0004
           bra   e_write

not04      anop
           cmp   #$0010                   ;devNotFound
           bne   not10
           lda   #e0010
           bra   e_write

not10      anop
           cmp   #$002b                   ;drvrWrtProt
           bne   not2b
           lda   #e002b
           bra   e_write

not2b      anop
           cmp   #$0040                   ;invalidPathame
           bne   not40
           lda   #e0040
           bra   e_write

not40      anop
           cmp   #$0044                   ;pathNotFound
           bne   not44
           lda   #e0044
           bra   e_write

not44      anop
           cmp   #$0046                   ;fileNotFound
           bne   not46
           lda   #e0046
           bra   e_write

not46      anop
           cmp   #$004c                   ;eofEncountered
           bne   not4c
           lda   #e004c
           bra   e_write

not4c      anop
           cmp   #$004d                   ;outOfRange
           bne   not4d
           lda   #e004d
           bra   e_write

not4d      anop
           cmp   #$004e                   ;invalidAccess
           bne   not4e
           lda   #e004e
           bra   e_write

not4e      anop
           bra   Done                     ;nothing to print

e_write    ANOP
           pea   e0004|-16                ;doesn't matter which
           pha
           _ErrWriteCString

Done       ANOP
           pea   crlf|-16
           pea   crlf
           _ErrWriteCString
;           brk   0                        ;debug
           lda   #$ffff                   ;put error result into acc
           sec
           rts

terrstr    dc    C'Tool Error $    ',H'00'
gerrstr    dc    C'GS/OS Error $    ',H'00'
crlf       dc    H'0d0a00'
e0004      dc    C' - bad param count (bug!)',H'00'
e0010      dc    C' - file (device) not found',H'00'
e002b      dc    C' - write protected',H'00'
e0040      dc    C' - invalid pathname',H'00'
e0044      dc    C' - path not found',H'00'
e0046      dc    C' - file not found',H'00'
e004c      dc    C' - reached EOF unexpectedly',H'00'
e004d      dc    C' - position out of range (archive possibly corrupted)',H'00'
e004e      dc    C' - file locked',H'00'

.OLD_ERR
           END


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
MyError    START
;
; Calling conventions:
;   JSR with error number in Acc.
;
; Handles YankIt errors (stuff like bad CRCs, etc).
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
           using globals

           pha                            ;used by Int2Hex
           sta   <last_error

           pea   errstr|-16
           pea   errstr+15
           pea   $0004
           _Int2Hex

           pea   errstr|-16
           pea   errstr
           _ErrWriteCString

           lda   <last_error
           sec
           sbc   #$0080
           asl   A
           asl   A
           tax
           lda   index+2,x
           pha
           lda   index,x
           pha
           _ErrWriteCString

Done       ANOP
           pea   crlf|-16
           pea   crlf
           _ErrWriteCString
           lda   #$ffff                   ;put error result into acc
           sec
           rts

errstr     dc    C'YankIt: Error $    : ',H'00'
crlf       dc    H'0d0a00'
index      dc    A4'e0080',A4'e0081',A4'e0082',A4'e0083'
           dc    A4'e0084',A4'e0085',A4'e0086',A4'e0087'
           dc    A4'e0088',A4'e0089',A4'e008a',A4'e008b'

e0080      dc    C'not a NuFX archive',H'00'
e0081      dc    C'master header is corrupted',H'00'
e0082      dc    C'unable to find start of next record',H'00'
e0083      dc    C'record header attributes > 1K',H'00'
e0084      dc    C'found more than 32 threads',H'00'
e0085      dc    C'found bogus record (no threads)',H'00'
e0086      dc    C'record header is corrupted',H'00'
e0087      dc    C'eof hit',H'00'
e0088      dc    C'LZW stack blown, archive is corrupted',H'00'
e0089      dc    C'requires GS/OS system 5.0.4 or better',H'00'
e008a      dc    C'bad CRC, file is corrupted',H'00'
e008b      dc    C'internal error: infinite loop during file creation',H'00'
           END


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
PrintMode  PRIVATE
;
; Calling conventions:
;   JSR
;
; Prints some info about what we think we're doing; used for debugging.
; Note that this expects the DP to be set up, so it'll be dangerous until
; StartUp has done some processing.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
           using globals

           lda   <mode
           and   #$00ff                   ;strip flag bits off
           asl   A
           asl   A
           tay
           lda   modear+2,y               ;push hi
           pha
           lda   modear,y                 ;push lo
           pha
           _WriteCString

           lda   <mode
           and   #dStdin
           bne   from_stdin

from_file  anop
           pea   file|-16
           pea   file
           bra   printit

from_stdin anop
           pea   stdin|-16
           pea   stdin

printit    anop
           _WriteCString

           lda   <mode
           and   #dVerbose
           beq   notverb
           pea   verbose|-16
           pea   verbose
           _WriteCString

notverb    anop
           lda   <mode
           and   #dNoSession
           bne   notsess
           pea   session|-16
           pea   session
           _WriteCString

notsess    anop
           rts

modear     dc    A4'0',A4'mode0',A4'mode1',A4'0',A4'mode2'
mode0      dc    C'--- Mode is View',H'00'
mode1      dc    C'--- Mode is Extract',H'00'
mode2      dc    C'--- Mode is Integrity',H'00'
verbose    dc    C'--- Verbose',H'0d0a00'
session    dc    C'--- Using GS/OS sessions',H'0d0a00'
file       dc    H'0d0a',C'--- Reading from file',H'0d0a00'
stdin      dc    H'0d0a',C'--- Reading from stdin',H'0d0a00'
           END


           ALIGN $100                     ;page-aligned
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
dpstack    START stack
           KIND  $12                      ;direct page/stack segment
;
; Stack and direct page for YankIt; we don't need the whole 1K.  If the
; stack does overflow, it'll go crashing into our direct page space, so
; we'll probably self-destruct before we hurt anybody else.
;
; (GS/OS sets the DP to the lowest location, and S to the highest location.)
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
directpage ENTRY
           ds    $100                     ;gimme a full page of DP
stack      ENTRY
           ds    $100                     ;only need a page
           END


           AGO   ._foo                    ;don't actually assemble this
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
Macros     START
;
; This is NOT a callable routine.  It's a collection of macros which exist
; solely to be found by macgen and moved into common.Macros.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; pick up some extra macros so we don't have to re-macgen
;           _DebugStatus                   ;debug
;           _DebugStr                      ;debug
;           _DebugVersion                  ;debug
;           _SetMileStone                  ;debug
;           writestr #"debug"              ;debug
;           writeln #"debug"               ;debug
;foo        _FlushGS foo                   ;debug
           _Int2Dec
           _Int2Hex
           _Long2Dec
           _Multiply
           _LongMul
           _LongDivide
           shortx
           longx
           END
._foo

