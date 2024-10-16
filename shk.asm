;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; YankIt v1.2 - Copyright (C) 1992 by Andy McFadden
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; shk.asm - uncompression routines and buffers
;
; Some of this code was adapted from the C implementation of the
; ShrinkIt LZW uncompression routines in NuLib.  Credits for the original
; sources go to Kent Dickey and Andy Nicholas.
;
; This will push up to 91 two-byte values onto the stack, so make sure
; you've got most of a full page of stack space.  (Actually, it shouldn't
; break 64 because of the RLE.)
;
; *** Dumb, dumb, dumb.  With LZW-I, it won't break 91 bytes.  With LZW-II,
; *** it can get much larger, because the table isn't cleared.  This will
; *** have to be fixed before the sources are released.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

           MLOAD YankIt.macros


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
ShrinkMem  START lzw
;
; Static tables and memory buffers for ShrinkIt LZW uncompression.  This
; is a code segment to keep things in the same ("lzw") bank.  Some of these
; buffers are used from "archive.asm" for general-purpose activities (and
; thus the buffer must be AT LEAST 8K in size).
;
; There's about 56K of static storage here, leaving 8K left over.  This
; could be hard to fit on a machine without lots of memory.
;
; You may be tempted to increase the pack buffer size to 40K ($a000).  I
; tried this, and did an integrity check on Moria.SHK (320K --> 550K) before
; and after.  I couldn't detect a difference (weird, but true... I even
; printed the size of each read to make sure it was working).  If you do
; change it, make sure you get it both here and in main.asm.  BTW, I did
; the check on an AppleDisk 3.5" drive, so HDs should show less effect.
; I'll try again when I get DMA SCSI...
;
; It is VERY important that this segment come first in this file.  Otherwise,
; other routines will get some strange random value for "table".  It's a
; bug in the assembler or linker, I guess.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; constants
;
dBlkSiz    gequ  4096                     ;4K chunk
dPakBufSiz equ   32768                    ;32K pack buffer (also in globals)

;
; This is the general-purpose buffer used for reading chunks of the file,
; both archive headers and compressed data.
;
buffer     ENTRY
           ds    dPakBufSiz               ;32k

;
; temporary buffers
;
lbuf       ENTRY
           ds    dBlkSiz+7                ;4k  for storing data after LZW
rbuf       ENTRY
           ds    dBlkSiz+7                ;4k  for storing data after RLE

;
; table of prefixes
; (4 bytes each - first two are unsigned char chr, second two are int prefix)
real_tab   ENTRY
           ds    dBlkSiz*4-256*4          ;15k

;
; This statement would have made life much easier, but unfortunately some
; part of APW screws it up every time.  I spent several hours trying to
; track down a bug caused by data overwriting itself, all because APW
; decided to make table offset from a DIFFERENT SYMBOL!!
;
; I was annoyed, to say the least.  So if you see "real_tab-1024", you can
; interpret it to be "table".  I suspect the problem may be in my version
; of the linker; I'll try again when Orca/M 2.0 arrives.
;
;table      gequ  real_tab-256*4           ;where full real_tab would start

;
; Tables for computing CRCs
;
crclo      ENTRY
           ds    256
crchi      ENTRY
           ds    256                      ;0.5k

;
; Mask table
;
mask_tab   ENTRY
           dc    H'00010303070707070f0f0f0f0f0f0f0f0f'

;
; bit count
;
number     ENTRY
           dc    I1'8,9,10,10,11,11,11,11'
           dc    I1'12,12,12,12,12,12,12,12,12'
           END


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
ExtractSHK START
;
; Calling conventions:
;   JSR with offset to thread header in "" bank in <ptr
;
; Main entry point for ShrinkIt LZW uncompression, and only routine in
; this file (besides InitCRCTab) that should be called directly from the
; "outside world".
;
; This routine prints the pretty messages, pulls the information out of
; the thread header, and then hands it off to MainLZW.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
           using globals
           using ParmBlocks

;           SetStone #"In ExtractSHK"      ;debug
           lda   <mode
           and   #dVerbose
           beq   msg_done
           pea   extmsg1|-16
           pea   extmsg1
           _WriteCString
           pea   filename_buf|-16
           pea   filename_buf+2
           _WriteCString
           ldy   <ptr
           lda   |$0004,y                 ;thread_kind
           cmp   #$0002                   ;resource fork?
           beq   rsrcmsg
           pea   extmsg2|-16
           pea   extmsg2
           _WriteCString
           bra   msg_done
rsrcmsg    pea   rsrcmsg2|-16
           pea   rsrcmsg2
           _WriteCString
msg_done   anop

           lda   #buffer                  ;set read to use compression buffer
           sta   pArcRead+$04
           lda   #buffer|-16
           sta   pArcRead+$06

           ldy   <ptr
           lda   |$0002,y                 ;thread_format
           cmp   #$0002                   ;LZW-I
           beq   lzw
           cmp   #$0003                   ;LZW-II
           beq   lzw

; we shouldn't be here at all... print msg and skip data
           pea   whoops|-16
           pea   whoops
           _ErrWriteCString

           ldy   <ptr
           lda   |$000e,y                 ;comp_thread_eof
           tax                            ;hi word
           lda   |$000c,y                 ;lo word
           jsr   SkipArchive              ;skip over all the stuff
           bra   Fail

lzw        anop
           sec
           sbc   #$0002
           sta   <type2                   ;== 0 for LZW-I, 1 for LZW-II

           lda   |$0006,y                 ;thread_crc
           sta   <thread_crc

           lda   |$0008,y                 ;thread_eof
           sta   <thread_eof
           sta   <othread_eof
           lda   |$000a,y
           sta   <thread_eof+2
           sta   <othread_eof+2
           lda   |$000c,y                 ;comp_thread_eof
           sta   <c_t_eof
           lda   |$000e,y
           sta   <c_t_eof+2

           jsr   MainLZW
           bcs   Fail

Done       ANOP
           lda   <mode
           and   #dVerbose
           beq   nodonemsg
           lda   <percount
           and   #$8000                   ;did we print a % message?
           bne   nopercmsg
           pea   backspc|-16
           pea   backspc
           _WriteCString
nopercmsg  anop
           pea   extdone|-16
           pea   extdone
           _WriteCString
nodonemsg  anop
;           SetStone #"Leaving ExtractSHK" ;debug
           clc
           rts

Fail       ANOP
;           SetStone #"Failed in ExtractSHK" ;debug
           sec
           rts

whoops     dc    C'Internal Error (bug!): bad call to ExtractSHK',H'0d0a00'
extmsg1    dc    C'Unshrinking ',H'00'
extmsg2    dc    C'...',H'00'
rsrcmsg2   dc    C' (resource fork)...',H'00'
extdone    dc    C'done.',H'0d0a00'
backspc    dc    H'0808080800'
           END


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
MainLZW    PRIVATE
;
; Calling conventions:
;   JSR with appropriate DP stuff initialized
;
; Extracts LZW-I or LZW-II threads.  Assumes that the pack buffer is
; less than 64K in size.
;
; Some self-modifying code is used for the MVN instruction.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
           using globals
           using ParmBlocks

           phb                            ;switch data bank to "lzw"
           shortm
           lda   #buffer|-16              ;get "lzw" bank
           pha
           plb
           longm

           lda   <type2                   ;init CRC; use "0" for LZW-I,
           eor   #$ffff                   ; "ffff" for LZW-II.
           inc   A
           sta   <crc

           lda   #$0101                   ;clear table
           sta   <entry
           stz   <reset_fix
           lda   #$ffff
           sta   <mask                    ;init LZW bit mask
           stz   <at_bit                  ;clear hi word of 8-bit data
           stz   <old_bit                 ; read with 16-bit index regs
           stz   <last_bit                ; (ditto)

           lda   #dPercFreq               ;init % msg countdown
           ora   #$8000                   ;set "first time" flag
           sta   <percount

;
; Compute size of initial read, and read data into the buffer.
;
           lda   <c_t_eof+2
           bne   huge
           lda   <c_t_eof
           cmp   #dPakBufSiz+1
           bge   huge

           sta   <toread                  ;toread = comp_thread_eof
           stz   <c_t_eof
           bra   firstread

huge       anop                           ;more than dPakBufSiz bytes left
           lda   #dPakBufSiz
           sta   <toread
           lda   <c_t_eof
           sec
           sbc   #dPakBufSiz
           sta   <c_t_eof
           lda   <c_t_eof+2
           sbc   #$0000
           sta   <c_t_eof+2

firstread  anop
;           writestr #"----- Initial read: " ;debug
;           lda   <toread                  ;debug
;           jsl   PrintNum                 ;debug

           lda   <toread                  ;read this many bytes
           jsr   LReadArchive
           bcc   initial_readok
           brl   Fail
initial_readok anop
           ldy   #buffer                  ;init ibuf to head of buffer
           sty   <ibuf                    ; (and leave it in Y-reg)

; find the CRC for this stuff
           lda   <type2
           bne   type2_crc

           lda   |$0000,y
           iny
           iny
           sta   <blkCRC
           bra   got_crc
type2_crc  anop
           lda   <thread_crc              ;we really should check the record's
           sta   <blkCRC                  ; version #, but it *should* be valid
got_crc    anop

           iny                            ;skip past disk volume #
           lda   |$0000,y                 ;get RLE escape char
           iny
           and   #$00ff                   ;strip off the high byte
           sta   <escape_char
           sty   <ibuf

;           writestr #"CRC / escape char:"  ;debug
;           lda   <blkCRC
;           jsl   PrintNum
;           lda   <escape_char
;           jsl   PrintNum                 ;DEBUG

;
; MAIN LOOP
;
; This runs until we're all done with this thread.
;
main_loop  ANOP
           lda   <thread_eof
           ora   <thread_eof+2
           bne   not_done
           brl   loop_done

not_done   anop
           ldy   <ibuf
           lda   <type2
           bne   hdrinf2

hdrinf1    anop
           lda   |$0000,y                 ;uncompressed length after RLE
           iny
           iny
           sta   <unlen
           sec                            ;does post-RLE size == full block?
           sbc   #dBlksiz                 ;if so, then no RLE was used (flag=0)
           sta   <rleflag                 ;if not, then RLE was used (flag!=0)
           lda   |$0000,y
           iny
           and   #$00ff                   ;0/1 if LZW used
           sta   <lzwflag
           bra   hdrinf_done

hdrinf2    anop
           lda   |$0000,y                 ;uncompressed length after RLE
           iny
           iny
           tax                            ;save temporarily
           and   #$1fff                   ;strip off extra stuff
           sta   <unlen
           sec                            ;does post-RLE size == full block?
           sbc   #dBlksiz                 ;if so, then no RLE was used (flag=0)
           sta   <rleflag                 ;if not, then RLE was used (flag!=0)
           txa
           and   #$8000                   ;hi bit indicates use of LZW
           sta   <lzwflag
           tax                            ;set Z-flag
           beq   hdrinf_done
           lda   |$0000,y                 ;if LZW was used, then the compressed
           iny                            ; length is also stored here
           iny
           sta   <complen

hdrinf_done anop
           sty   <ibuf

;           writestr #"length after RLE: " ;debug
;           lda   <unlen
;           jsl   PrintNum
;
;           writestr #"lzw/rle flags: "    ;debug
;           ldx   <lzwflag
;           lda   <rleflag
;           jsl   PrintLong
;
;           lda   <type2                   ;debug
;           beq   npr
;           lda   <lzwflag
;           beq   npr
;           writestr #"len after rle+lzw: "
;           lda   <complen
;           jsl   PrintNum                 ;debug
;npr        anop                           ;debug

;
; Okay, now we have some information on the next block.  Since implementing
; a circular buffer would slow down the uncompression routines, we just shift
; everything down when it looks like we're about to run out of room.
;
; (an alternative is to ALIGN $10000 the buffer and then AND $7fff, but
; that would require tya/and/tay after every byte fetch.  Another alternative
; is to make the buffer 64K and let it roll over, but that requires a pristine
; 64K bank and some complicated logic around the read & write commands.  These
; alternatives wouldn't save us much anyway; they can be considered part of
; the disk access overhead, which certainly dwarfs the MVN.)
;
; If we're using LZW-I, we really don't know how much space the compressed
; thread takes up (unless no LZW was used, in which case "unlen" will tell us).
; If we're using LZW-II and we packed the next chunk with lzw, then "complen"
; will tell us.
;
; In either case, we want to add a few bytes on so that the header bytes from
; the next compressed chunk will be in the buffer when we come 'round again.
;

;           stz   <toread                  ;DEBUG (acts as a flag)
; Step 1: see if we have enough room
; (space is the amount of space left in the buffer, NOT the amount of valid
; data left.  Doesn't matter here.)
           lda   #buffer+dPakBufSiz       ;offset of end of pack buffer
           sec
           sbc   <ibuf                    ;subtract current position
           sta   <space
           lda   <type2
           bne   space2

space1     anop
           lda   <unlen                   ;this is usually larger than needed
           clc
           adc   #$0006                   ;3/4 byte header + 2 just in case
           cmp   <space
           bge   moveit                   ;we need more than we got
           brl   move_done                ;we got plenty, get on with it

space2     anop
           lda   <lzwflag
           beq   space1                   ;no LZW this block, so use <unlen
           lda   <complen                 ;this is exactly right, mostly
           clc
           adc   #$0010                   ;+6 for header, +10 'cause it's weird
           cmp   <space
           blt   move_done                ;data needed < data left, don't move

; Step 2: move stuff around and read more if needed
moveit     ANOP
; Sanity check: if there's nothing left to read, then moving data around
; isn't going to help anything.
           lda   <c_t_eof
           ora   <c_t_eof+2
           beq   move_done

; Here's where life gets unpleasant.  We need to move the existing data down,
; and fill the remaining part of the buffer, being careful not to read too
; much in, or overlap anything that already exists.
;
; We could get fancy here and use, say, 8 pairs of lda abs,x/sta abs,x
; instructions, taking the cycles per byte from 7 down to say 3.  If you
; figure that, on average, we move 2048 bytes, this is a savings of 8192
; cycles per disk read.  Compared with >50K cycles for UndoRLE, it doesn't
; even matter...  You might shave 0.5 seconds off of a minute-long extraction.
; 2048 is far too high; the average after compression is likely 1024 or so.
           lda   #buffer|-16              ;bank addr
           xba                            ;change 00xy to xy00
           ora   #buffer|-16              ;change to xyxy
           sta   >mvnop+1                 ;move in same bank
           ldx   <ibuf                    ;source addr
           ldy   #buffer                  ;dest addr
           lda   <space                   ;count
mvnop      mvn   $ff,$ff                  ;(operands were filled in above)

           lda   #dPakBufSiz
           sec
           sbc   <space
           sta   <tmp

           lda   <c_t_eof+2               ;can we fill the buffer again?
           bne   rhuge                    ;>64K left, so yes
           lda   <c_t_eof
           cmp   <tmp                     ;compare with how much space left
           beq   rsmall                   ;exact match, read it all
           bge   rhuge                    ;yes, lots left
;          blt   rsmall

rsmall     anop
           sta   <toread
           stz   <c_t_eof
           bra   readit

rhuge      anop
           lda   <tmp                     ;dPakBufSiz - space
           sta   <toread
           lda   <c_t_eof
           sec
           sbc   <tmp
           sta   <c_t_eof
           lda   <c_t_eof+2
           sbc   #$0000
           sta   <c_t_eof+2

readit     anop
           lda   #buffer
           sta   <ibuf                    ;reset ibuf to start of buffer
           clc
           adc   <space
           sta   >pArcRead+$04            ;set buffer at end of old data
           lda   <toread
           jsr   LReadArchive
           bcc   read_ok
           brl   Fail
read_ok    anop

move_done  ANOP
;           lda   <toread                  ;debug
;           beq   no_read                  ;debug
;           writestr #"read "              ;debug
;           lda   <toread                  ;debug
;           jsl   PrintNum                 ;debug
;no_read    anop                           ;debug

;
; Figure out how much of the output data is important - all 4096 bytes,
; or just a few.
;
           lda   <thread_eof+2
           bne   use_part                 ;>64K left to write
           lda   <thread_eof
           cmp   #dBlksiz+1
           blt   use_all                  ;<=4096 bytes left

use_part   anop
           lda   #dBlksiz
           sta   <partial
           lda   <thread_eof
           sec
           sbc   #dBlksiz
           sta   <thread_eof
           lda   <thread_eof+2
           sbc   #$0000
           sta   <thread_eof+2
           bra   doit

use_all    anop
           lda   <thread_eof
           sta   <partial
           stz   <thread_eof

doit       anop
;           lda   <partial                 ;debug
;           jsl   PrintNum                 ;debug

;
; Now we have all that we need.  Call the uncompression routines on this
; chunk.  There are three values to consider: lzwflag, rleflag, and type2.
;
           lda   <lzwflag
           bne   use_lzw

dont_use_lzw anop
           lda   <rleflag
           bne   dz_use_rle

dz_dont_use_rle anop                      ;no compression at all!
           lda   <ibuf
           sta   <wrbuf                   ;set write buffer = current
           clc
           adc   <partial                 ;advance past the data
           sta   <ibuf
           lda   #$0101                   ;clear table when no LZW-II here
           sta   <entry
           stz   <reset_fix
           bra   uncomp_done

dz_use_rle anop                           ;only RLE on this chunk
           lda   <ibuf
           tax                            ;unpack from ibuf...
           clc
           adc   <unlen                   ;skip over RLE-only data
           sta   <ibuf
           ldy   #rbuf                    ;...to rbuf
           sty   <wrbuf                   ;write from rbuf

           jsr   UndoRLE                  ;do it

           lda   #$0101                   ;clear table when no LZW-II here
           sta   <entry
           stz   <reset_fix
           bra   uncomp_done

use_lzw    anop
           lda   <rleflag
           bne   uz_use_rle

uz_dont_use_rle anop                      ;just LZW for this one
           lda   #lbuf                    ;write from lbuf
           sta   <wrbuf
           lda   <type2
           bne   unp_2
           jsr   UndoLZW_1                ;unpack unlen bytes from ibuf to lbuf
           bcs   lzw_failed               ;usually a blown stack
           bra   uncomp_done
unp_2      anop
           jsr   UndoLZW_2
           bcs   lzw_failed
           bra   uncomp_done

uz_use_rle anop                           ;both RLE and LZW here
           lda   <type2
           bne   runp_2
           jsr   UndoLZW_1                ;LZW unlen bytes from ibuf to lbuf
           bcs   lzw_failed
           ldx   #lbuf                    ;do RLE from lbuf to rbuf
           ldy   #rbuf                    ;write from rbuf
           sty   <wrbuf
           jsr   UndoRLE                  ;RLE 'til 4K
           bra   uncomp_done
runp_2     jsr   UndoLZW_2
           bcs   lzw_failed
           ldx   #lbuf                    ;do RLE from lbuf to rbuf
           ldy   #rbuf                    ;write from rbuf
           sty   <wrbuf
           jsr   UndoRLE
           bra   uncomp_done

lzw_failed ANOP
           pea   lzw_fail|-16
           pea   lzw_fail
           _ErrWriteCString
           brl   Fail

uncomp_done ANOP
;
; Now all we have to do is compute the CRC and write out the chunk.  First we
; figure out if we want to work on all 4K or just part of it.
;
           lda   <type2
           bne   crc_2
crc_1      anop                           ;type 1: CRC on all 4096 bytes
           lda   <wrbuf
           ldy   #dBlksiz
           jsr   LCalcCRC                 ;update <crc
           bra   crc_done
crc_2      anop                           ;type 2: CRC only on valid data
           lda   <wrbuf
           ldy   <partial
           jsr   LCalcCRC                 ;update <crc
crc_done   ANOP

           lda   <mode
           and   #dIntegrity
           bne   write_ok

; write it
           lda   <wrbuf
           sta   >pWrite+$04
           lda   #buffer|-16
           sta   >pWrite+$06
           lda   <partial
           jsr   LWriteFile
           bcc   write_ok
;           jsr   LToolError              ;(error reported by WriteFile)
           brl   Fail

write_ok   anop
;           lda   >pWrite+$02              ;DEBUG
;           sta   >pFlush+$02
;           _FlushGS pFlush
;           bcc   _pastflush
;           jsr   LToolError
;           writeln #"flush failed"
;           bra   _pastflush
;pFlush     anop
;           dc    I2'1'
;           dc    I2'1'
;_pastflush anop                           ;DEBUG

           lda   <mode
           and   #dVerbose
           beq   nopmsg

           jsr   PrintPerc

nopmsg     anop

; One down, possibly some more to go.  Go back and do it again.
           jmp   main_loop

loop_done  ANOP
           lda   <blkCRC
           cmp   <crc
           beq   crc_match

;           writeln #"crc mismatch:"       ;debug
;           lda   <crc                     ;debug
;           jsl   PrintNum                 ;debug
;           lda   <blkCRC                  ;debug
;           jsl   PrintNum                 ;debug

           lda   #eBadThread              ;but keep trying anyway
           jsr   LMyError                 ; (don't branch to Fail)

crc_match  ANOP

Done       ANOP
           plb
           clc
           rts

Fail       ANOP
           pea   abort_msg|-16
           pea   abort_msg
           _ErrWriteCString
           plb
           sec
           rts

lzw_fail   dc    C'warning: LZW stack overflow, file was corrupted',H'0d0a00'
abort_msg  dc    C'Aborting.',H'0d0a00'
           END


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
UndoLZW_1  PRIVATE
;
; Calling conventions:
;   JSR
;
; Uncompresses LZW from ibuf to lbuf; stops after "unlen" bytes have been
; extracted.  The main loop of this routine should be fast.
;
; In C, this routine looks something like:
;
;static void
;undo_LZW(buffer, length)
;unsigned char *buffer;  /* where to put output */
;int length;  /* uncompressed length of output */
;{
;    register int oldc, incode, finalc, ptr;
;
;    /* initialize variables */
;    Table = Real_tab-256;
;    entry = 0x101;  /* start at $101 */
;    at_bit = at_byte = 0;
;    out_bytes = 0;
;    stack_ptr = 0;
;
;    last_byte = 0;  /* init last_byte */
;    oldc = incode = get_code(/*buffer*/);
;    finalc = (oldc & 0xff);
;    *(buffer + out_bytes++) = (unsigned char) incode;
;
;    /* main loop */
;    while (out_bytes < length) {
;        incode = ptr = get_code(/*buffer*/);
;        if (ptr >= entry) {
;            push(finalc);
;            ptr = oldc;
;        }
;        while (ptr > 0xff) {
;            push(Table[ptr].chr);
;            ptr = Table[ptr].prefix;
;        }
;
;        /* ptr is now < 0x100 */
;        finalc = ptr;
;        *(buffer++) = (onebyt) finalc;
;        while (stack_ptr > stack_start)
;                *(buffer++) = *(--stack_ptr);
;        Table[entry].chr = (finalc & 0xff);  /* mask to get unsigned?? byte */
;        Table[entry].prefix = oldc;
;        entry++;
;        oldc = incode;
;    }
;}
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
           using globals

;           SetStone #"In UndoLZW_1"       ;debug
;           writeln #"--- UndoLZW_1"       ;debug
           tsx
           stx   <old_stack               ;in case we blow ours

;
; Initialize variables
;
           lda   #$0101
           sta   <entry
           stz   <reset_fix               ;LZW-II decoding glitch fix
           stz   <at_bit
           stz   <at_byte
           stz   <last_byte

           jsr   GetCode
           sta   <oldc
           sta   <incode
           sta   |lbuf
           and   #$00ff
           sta   <finalc                  ;:finalc = (oldc & 0xff);

           lda   #$0001                   ;this and "sta lbuf" above make up
           sta   <out_bytes               ; :*(buffer+out_bytes++) = incode;

;
; main loop
;
main_loop  ANOP
;           lda   <out_bytes              ;(move to end of loop... if unlen
;           cmp   <unlen                  ; equals out_bytes at the outset,
;           bge   loop_done               ; we're in a heap of trouble...)

           jsr   GetCode
           ldy   #$0000                   ;stack starts out empty
           sta   <incode
           cmp   <entry                   ;:if (ptr >= entry) {
           blt   pt_ngt_e

           pei   <finalc                  ;:push(finalc)
           iny
           lda   <oldc
pt_ngt_e   anop

           cmp   #$0100
           blt   smallptr

ploop      anop
           asl   A                        ;4 bytes per "table" entry
           asl   A
           tax
           lda   |real_tab-1024,x         ;:push(Table[ptr].chr);
           pha
           iny
           cpy   #100                     ;+ stack check - this could be
           bge   Blown                    ;+ removed, but I don't recommend it
           lda   |real_tab-1024+2,x       ;:ptr = Table[ptr].prefix;
           cmp   #$0100
           bge   ploop                    ;blt/bra?  what was I thinking??
ploop_done anop

smallptr   anop

; ptr is now < $0100
           sta   <finalc                  ;:finalc = ptr;
           ldx   <out_bytes
           sta   |lbuf,x
           inx
           tya                            ;set flags for y; faster than cpy
           beq   just_one                 ;just one byte; stack was empty

dloop      anop
           pla
           sta   |lbuf,x                  ;we only care about the low byte
           inx
           dey
           bne   dloop
just_one   anop
           stx   <out_bytes

           lda   <entry
           asl   A
           asl   A
           tax
           lda   <finalc
           and   #$00ff
           sta   |real_tab-1024,x         ;:Table[entry].chr = (finalc & 0xff);
           lda   <oldc
           sta   |real_tab-1024+2,x       ;:Table[entry].prefix = oldc
           inc   <entry                   ;:entry++
           lda   <incode
           sta   <oldc

;kloop      lda   >$e0c000                 ;DEBUG
;           bpl   kloop                    ;DEBUG
;           sta   >$e0c010                 ;DEBUG

           lda   <out_bytes               ;look at how much we've done
           cmp   <unlen                   ;length expected
           blt   main_loop                ;ain't there yet

loop_done  ANOP
; nothing special to do; just exit quietly, content in a job well done.

Done       ANOP
;           SetStone #"Leaving UndoLZW_1"  ;debug
           clc
           rts

Blown      ANOP
           lda   #eStackBlown
           jsr   LMyError
;           writestr #"out_bytes was: "    ;debug
;           lda   <out_bytes               ;debug
;           jsl   PrintNum                 ;debug
           ldx   <old_stack
           txs
           sec
           rts
           END


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
UndoLZW_2  PRIVATE
;
; Calling conventions:
;   JSR
;
; Uncompresses LZW-II from ibuf to lbuf; stops after "unlen" bytes have
; been extracted.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
           using globals

;           SetStone #"In UndoLZW_2"       ;debug
;           writeln #"--- UndoLZW_2"       ;debug
           tsx
           stx   <old_stack               ;in case we blow ours

;           shortm                         ;stupid assembler/linker won't let
;           lda   #lbuf|-16                ; me code this in directly
;           sta   >_move+1
;           longm

;
; Initialize variables
;
           stz   <at_bit
           stz   <at_byte
           stz   <last_byte
           stz   <out_bytes

;
; main loop
; (actually, this first bit of crud is the table clear code, which was
; originally in the main loop.)
;
           lda   <entry                   ;(could lda #$0101, cmp, bit, fall)
           cmp   #$0101                   ;table is empty?
           bne   main_loop
           lda   <reset_fix               ;did we set oldc & incode last time?
           bne   fmain_loop               ;yes, so plow right in
           bra   ct2                      ;saves a few cycles

clear_table anop
           lda   #$0101                   ;resetting <entry clears table
           sta   <entry
           stz   <reset_fix               ;(LZW-II decoding glitch fix)
ct2        anop                           ;...and we'd have done all this stuff
           jsr   GetCode                  ; on the next pass through the loop
           sta   <oldc                    ; anyway, so...
           sta   <incode
           ldx   <out_bytes
           sta   |lbuf,x                  ;:*(buffer+out_bytes++) = incode;
           inx
           stx   <out_bytes
           and   #$00ff
           sta   <finalc                  ;:finalc = (oldc & 0xff);

; fix the soon-to-be-famous LZW-II bug
; (problem arises when a table clear is the second-to-last code in a chunk.
; the code read above is thus the first building block of the new table,
; but it isn't added to the table, so <entry doesn't change.  Thus we have
; to make sure that we don't do too many or too few GetCode calls.)
           cpx   <unlen                   ;length expected
           blt   main_loop                ;not done yet
           inc   <reset_fix               ;tell the world that we already have
;           brk   0                        ;debug
;           bra   loop_done                ; the first code, and exit
           jmp   loop_done

fmain_loop stz   <reset_fix
main_loop  ANOP
           jsr   GetCode
           ldy   #$0000                   ;stack starts out empty
           sta   <incode                  ;:incode = ptr = get_code();
           cmp   #$0100                   ;is it a table clear code?
           blt   smallptr                 ;just a character
           beq   clear_table              ;yes, so break out and handle it

           cmp   <entry                   ;:if (ptr >= entry) {
           blt   pt_ngt_e                 ; (checking for KwKwK)

;           shortm                         ;MVFIX
           pei   <finalc                  ;:push(finalc)  (always 16 bit)
;           pla                            ;MVFIX
;           longm                          ;MVFIX
           iny
           lda   <oldc
           cmp   #$0100
           blt   smallptr
pt_ngt_e   anop

ploop      anop
           asl   A                        ;4 bytes per "table" entry
           asl   A
           tax
;           shortm                         ;MVFIX
           lda   |real_tab-1024,x         ;:push(Table[ptr].chr);
           pha
;           longm                          ;MVFIX
           iny
           cpy   #100                     ;+ stack check - this could be
           bge   Blown                    ;+ removed, but I don't recommend it
           lda   |real_tab-1024+2,x       ;:ptr = Table[ptr].prefix;
           cmp   #$0100
           bge   ploop
ploop_done anop

smallptr   anop
; ptr is now < $0100
           sta   <finalc                  ;:finalc = ptr;
           ldx   <out_bytes
           sta   |lbuf,x
           inx
           tya                            ;set flags for y; faster than cpy
           beq   just_one                 ;just one byte; stack was empty

;           txa                            ;2
;           clc                            ;2
;           adc   #lbuf                    ;4
;           tax                            ;2
;           tya                            ;2 #of bytes to move
;           dec   A                        ;2 (-1 for MVN)
;           txy                            ;2 destination start address
;           tsx                            ;2
;           inx                            ;2 source start address (bank 0)
;_move      mvn   $00,$ff                  ;7 * byte count
;           dex                            ;2
;           txs                            ;2 new stack pointer
;           tya                            ;2 new end of buffer
;           sec                            ;2
;           sbc   #lbuf                    ;4
;           tax                            ;2
;           anop                           ;-- 34 cycles of overhead, need 4+

dloop      anop
           pla                            ;5
           sta   |lbuf,x                  ;6 we only care about the low byte
           inx                            ;2
           dey                            ;2
           bne   dloop                    ;3/2
just_one   anop
           stx   <out_bytes

           lda   <entry
           asl   A
           asl   A
           tax
           lda   <finalc
           and   #$00ff
           sta   |real_tab-1024,x         ;:Table[entry].chr = (finalc & 0xff);
           lda   <oldc
           sta   |real_tab-1024+2,x       ;:Table[entry].prefix = oldc
           inc   <entry                   ;:entry++
           lda   <incode
           sta   <oldc

;kloop      lda   >$e0c000                 ;DEBUG
;           bpl   kloop                    ;DEBUG
;           sta   >$e0c010                 ;DEBUG

           lda   <out_bytes               ;output so far
           cmp   <unlen                   ;length expected
           blt   main_loop

loop_done  ANOP
; nothing special to do; just exit quietly, content in a job well done.

Done       ANOP
;           SetStone #"Leaving UndoLZW_2"  ;debug
           clc
           rts

Blown      ANOP
           lda   #eStackBlown
           jsr   LMyError
;           writestr #"out_bytes was: "    ;debug
;           lda   <out_bytes               ;debug
;           jsl   PrintNum                 ;debug
           ldx   <old_stack
           txs
           sec
           rts

           END


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
GetCode    PRIVATE
;
; Calling conventions:
;   JSR
;   Returns the code in Acc
;
; Gets the next LZW code.
;
; This routine is called more often than any other.  It is vital that
; this be as efficient as possible.
;
; In C, this routine looks something like:
;
;static int
;get_code()
;{
;    register unsigned int num_bits, old_bit, last_bit;
;    long value, mask;
;    onebyt byte1, byte2, byte3;  /* get compressed chars... */
;
;    num_bits = ((entry+1) >> 8);  /* get hi-byte of entry */
;    last_bit = at_bit + Number[num_bits] + 8;
;    old_bit = at_bit;
;
;    if (at_bit == 0)
;        last_byte = Getc();
;
;    byte1 = last_byte;                       /* first byte = last one used */
;    last_byte = byte2 = getc();              /* get a second */
;    if (last_bit > 16) {                     /* do we need a third? */
;        last_byte = byte3 = getc();
;    } else {
;        byte3 = 0;
;    }
;    value = ((((long)byte3 << 8) + (long)byte2) << 8) + (long)byte1;
;
;    mask = (long) Mask_tab[num_bits];
;    at_byte += (last_bit >> 3);  /* new byte */
;    at_bit = (last_bit & 0x07);
;
;    if (old_bit)
;        return ((value >> old_bit) & mask);
;    else
;        return (value & mask);  /* shifting by zero may be undefined */
;}
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
           using globals

           ldy   <ibuf                    ;and we keep this throughout

           lda   <entry
           inc   A
           xba
           and   #$00ff                   ;clear high (for TAX later)
           shortm                         ;:(entry+1) >> 8
;           sta   <num_bits
           tax

           lda   |mask_tab,x              ;:mask = Mask_tab[num_bits]
           sta   <mask+1                  ;low byte should already be $ff

           lda   |number,x
           clc
           adc   <at_bit                  ;this is 8 bits (actually 0-7)
;           adc   #$08
           sta   <last_bit                ;:last_bit=at_bit+Number[num_bits]+8

           lda   <last_byte               ;Acc holds last_byte for a while
           ldx   <at_bit                  ;danger: 8 bit data, 16 bit reg
           stx   <old_bit
           bne   at_bit_nz
           lda   |$0000,y                 ;:last_byte = getc()
           iny
at_bit_nz  anop

           sta   <value                   ;:byte1 = last_byte;
           lda   |$0000,y
           iny
           sta   <value+1                 ;:last_byte = byte2 = Getc()
           stz   <value+2                 ;:byte3 = 0 (same speed as BRA past)

           ldx   <last_bit                ;:if (last_bit > 16) {
           cpx   #17                      ; (last_bit set to $0000 in MainLZW)
           blt   gotenough

           lda   |$0000,y                 ;:byte3 = last_byte = getc()
           iny
           sta   <value+2

gotenough  anop
           sta   <last_byte               ;free Acc

;           lda   <last_bit                ;update at_bit
           txa
           and   #$07
           sta   <at_bit                  ;:at_bit = (last_bit & 0x07)

; at this point, we have a nice result in the first three bytes of "value".
; we now need to shift it into position and then AND it with a mask.
           longm
           sty   <ibuf                    ;save the ibuf pointer

;           lda   <value
;           ldx   <old_bit
;           beq   no_shift
;shloop     anop
;           lsr   <value+2                 ;rotate from hi word
;           ror   A                        ;down into Acc (low word)
;           dex
;           bne   shloop
;no_shift   anop

           lda   <old_bit                 ;ranges from 0-7
           beq   no_shift
           asl   A
           tax
           lda   <value
           jmp   (barrel,x)
no_shift   anop
           lda   <value
           bra   bottom_of

barrel     anop
           ds    2
           dc    A2's1'
           dc    A2's2'
           dc    A2's3'
           dc    A2's4'
           dc    A2's5'
           dc    A2's6'
           dc    A2's7'
           dc    A2'whoops'               ;too far
whoops     anop
           brk   $ff

s7         lsr   <value+2                 ;rotate from hi word
           ror   A                        ;down into Acc (low word)
s6         lsr   <value+2
           ror   A
s5         lsr   <value+2
           ror   A
s4         lsr   <value+2
           ror   A
s3         lsr   <value+2
           ror   A
s2         lsr   <value+2
           ror   A
s1         lsr   <value+2
           ror   A
bottom_of  anop

           and   <mask                    ;:return ((value >> old_bit) & mask)
;... and we return the value in Acc as the result.

           rts
           END


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
UndoRLE    PRIVATE
;
; Calling conventions:
;   JSR with source buffer in X-reg and destination buffer in Y-reg
;
; Unpacks RLE stuff.  This has to chew through 4K of data; if nothing
; else, it should be reasonably efficient at copying it.  Stops when it
; has unpacked dBlksiz bytes (4096).
;
; Uses self-modifying code for efficiency.  Requires 50-75000 cycles to
; complete.  Using txs/tsx to save the X register would save 4 cycles
; per series (NOT per byte), but would require turning off interrupts for
; roughly 1/60th of a second.
;
; The RLE pattern is:  <DLE> char count  (count=0 is 1 char, count=255 is 256)
;
; DP USAGE:
;   subtmp, subtmp2
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
           using globals

;           SetStone #"In UndoRLE"         ;debug
;           writeln #"--- UndoRLE"         ;debug

           txa
           sta   >_get1+1                 ;init source and dest operands
           sta   >_get2b+1
           inc   A                        ;addr+1 for this one
           sta   >_get2a+1
           tya
           sta   >_put1+1
           sta   >_put2+1

           ldx   #$0000
           txy                            ;destination index

           shortm
           stz   <subtmp+1                ;clear hi byte of <subtmp
           lda   <escape_char
           sta   >_cmpesc+1               ;hey, it's one cycle * 4K at worst

;
; Loop until we output dBlksiz characters.
;
           bra   loop

single     anop
_put1      sta   |$0000,y                 ;5  about 18 cycles/byte for singles
           iny                            ;2
           cpy   #dBlksiz                 ;3
           bge   Done                     ;2/3

loop       ANOP
_get1      lda   |$0000,x                 ;4+
           inx                            ;2
_cmpesc    cmp   #$00                     ;2  usually $db
           bne   single                   ;3/2

series     anop
_get2a     lda   |$0001,x                 ;4+ get count
           sta   <subtmp                  ;3  hang onto it
_get2b     lda   |$0000,x                 ;4+ get char
           inx                            ;2  advance past char & count
           inx                            ;2
           stx   <subtmp2                 ;4  save output index
           ldx   <subtmp                  ;3  retrieve count (hi byte cleared)

sloop      anop                           ;   about 12 cycles/byte for series
_put2      sta   |$0000,y                 ;5  (plus overhead)
           iny                            ;2
           dex                            ;2
           bpl   sloop                    ;3/2 X-reg is 16 bits, so this is ok

           ldx   <subtmp2                 ;4

;           bra   loop_end                 ;3
;single     anop
;_put1      sta   |$0000,y                 ;5  about 18 cycles/byte for singles
;           iny                            ;2
;
;loop_end   ANOP
;           cpy   #dBlksiz                 ;3
;           blt   loop                     ;3/2


           cpy   #dBlksiz                 ;3
           blt   loop                     ;3/2

Done       ANOP
           longm
           cpy   #dBlkSiz
           beq   okdone
           pea   badrle|-16
           pea   badrle
           _ErrWriteCString

okdone     anop
;           SetStone #"Leaving UndoRLE"    ;debug
           rts

badrle     dc    C'warning: RLE uncompress failed, archive may be corrupted'
           dc    H'0d0a00'
           END


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
PrintPerc  START
;
; Calling conventions:
;   JSR
;
; Prints the percentage completed; this is called from both LZW routines
; and uncompressed extract.  Uses <percount to decide whether or not to
; print anything.
;
; Reminder: B != K
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
           using globals

           dec   <percount
           lda   <percount
           and   #$7fff                   ;strip flag
           beq   doit
           jmp   Done

doit       anop
           pea   $0000                    ;64-bit result (caramba!)
           pea   $0000
           pea   $0000
           pea   $0000
           pei   <thread_eof+2            ;amount left to do
           pei   <thread_eof
           pea   $0000
           pea   100                      ;multiply thread_eof * 100
           _LongMul
           plx                            ;yank part of result we want
           ply
           pla                            ;throw out high 32 bits
           pla
           pea   $0000                    ;push space for LongDivide remainder
           pea   $0000
           pea   $0000                    ;and quotient
           pea   $0000
           phy                            ;stuff this back on as dividend
           phx
           pei   <othread_eof+2           ;divide by original thread_eof
           pei   <othread_eof
           _LongDivide
           pla                            ;this is the quotient (lo)
           ply                            ;quotient (hi)
           ply                            ;remainder
           ply
           eor   #$ffff                   ;make it negative
           inc   A
           clc
           adc   #100                     ;this gives us 100-result
           pha                            ;push back on for _Int2Dec
           pea   percstr|-16
           pea   percstr
           pea   $0003                    ;at most 3 digits
           pea   $0000
           _Int2Dec

           shortm
           lda   >percstr+1               ;check for space; make 5% == 05%
           cmp   #" "
           bne   p_nospc
           lda   #"0"
           sta   >percstr+1
p_nospc    anop
           lda   #"%"                     ;the coup de grace
           sta   >percstr+3
           longm

           lda   <percount
           and   #$8000
           bne   first
           pea   backspc|-16
           pea   backspc
           _WriteCString
first      pea   percstr|-16
           pea   percstr
           _WriteCString

           lda   #dPercFreq               ;also clears hi bit
           sta   <percount

Done       ANOP
           rts

backspc    dc    H'0808080800'
percstr    dc    C'   %',H'00'
           END


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
InitCRCTab START
;
; Calling conventions:
;   JSR
;
; Initializes the CRC table.  Routine by Andy Nicholas.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
           using globals

           sep   #$30                     ;easier to do at byte level
           LONGA OFF
           LONGI OFF

           phb
           lda   #^crclo                  ;get bank addr of crc tables
           pha
           plb                            ;switch to that bank

; zero everything
           ldx   #$00
zeroLoop   stz   |crclo,x
           stz   |crchi,x
           inx
           bne   zeroLoop

; the following is the normal bitwise computation tweaked a little to
; work in the table-maker.

docrc      anop
           ldx   #$00                     ;number to do CRC for

fetch      txa
           eor   |crchi,x                  ;add byte into high of crc
           sta   |crchi,x

           ldy   #$08                     ;do 8 bits
loop       asl   |crclo,x                  ;shift current crc-16 left
           rol   |crchi,x
           bcc   loop1

; if previous high bit wasn't set, then don't add crc polynomial ($1021)
; into the cumulative crc; else, add it.

           lda   |crchi,x                  ;add hi part of crc poly into
           eor   #$10                     ; cumulative crc hi
           sta   |crchi,x

           lda   |crclo,x                  ;add lo part of crc poly into
           eor   #$21                     ;cumulative crc lo
           sta   |crclo,x
loop1      dey                            ;do next bit
           bne   loop                     ;done?  nope, loop

           inx                            ;do next number in series (0-255)
           bne   fetch                    ;didn't roll over, so fetch more

           rep   #$30                     ;back to 16 bits
           LONGA ON
           LONGI ON

           plb
           rts                            ;done
           END

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
LCalcCRC   PRIVATE
;
; Calling conventions:
;   JSR with the offset into the "lzw" bank in Acc, #of bytes to CRC in
;     the Y-reg, and the initial crc value in <crc
;   The result is left in <crc
;
; Computes a CRC on a range of bytes in the "lzw" bank.  Expects B to
; be set to the "lzw" bank; if not, it won't find the crclo/crchi tables.
;
; Some self-modifying code is used.
;
; DP USAGE:
;   subtmp
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
           using globals

           sta   >_fetch+1                ;ick!  set start
           sty   <subtmp                  ;save #of bytes to do
           lda   #$0000                   ;want hi byte zero for TAX
           tay                            ;set Y to zero
           shortm                         ;manipulations are on bytes

loop       ANOP
_fetch     lda   |$0000,y                 ;<-- this gets changed to start
           eor   <crc+1                   ;add byte into crc hi byte
           tax                            ; to make offset into tables
           lda   <crc                     ;get previous lo byte back
           eor   |crchi,x                 ;add it to the proper table entry
           sta   <crc+1                   ;save it

           lda   |crclo,x                 ;get new lo byte
           sta   <crc                     ;save it back

           iny                            ;iny/cpy takes 6 cycles, +1 for
           cpy   <subtmp                  ; index crossing page boundary
           blt   loop                     ;can't just dec/bne because of shortm

           longm
           rts
           END


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
LReadArchive PRIVATE
;
; Calling conventions:
;   JSR (same as ReadArchive)
;
; Switches back to the "" data bank, and calls ReadArchive.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
           phb
           phk
           plb
           jsr   ReadArchive
           plb
           rts
           END

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
LWriteFile START
;
; Calling conventions:
;   JSR (same as WriteFile)
;
; Switches back to the "" data bank, and calls WriteFile.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
           phb
           phk
           plb
           jsr   WriteFile
           plb
           rts
           END

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
LToolError PRIVATE
;
; Calling conventions:
;   JSR (same as ToolError)
;
; Switches back to the "" data bank, and calls ToolError.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
           phb
           phk
           plb
           jsr   ToolError
           plb
           rts
           END

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
LMyError   PRIVATE
;
; Calling conventions:
;   JSR (same as MyError)
;
; Switches back to the "" data bank, and calls MyError.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
           phb
           phk
           plb
           jsr   MyError
           plb
           rts
           END

