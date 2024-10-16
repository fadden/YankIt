;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; archive.asm - operations on the NuFX archive
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

           MLOAD YankIt.macros


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
ProcessMHBlock START
;
; Calling conventions:
;   JSR
;   Returns with carry set on error (CRC problems, etc).
;
; Reads the master header block, and interprets various fields.  Among
; other things, this makes sure that the file we're reading is actually
; a NuFX archive.
;
; If mode is "view", print some header lines.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
           using globals
           using ParmBlocks

;           SetStone #"About to read"      ;debug
retry      ANOP
           lda   #mhbuf                   ;read into the master header buffer
           sta   pArcRead+$04
           lda   #mhbuf|-16
           sta   pArcRead+$06
           lda   #$0001                   ;cache header blocks
           sta   pArcRead+$10             ;cachePriority
           lda   #48                      ;size of master header
           jsr   ReadArchive              ;read it
           bcc   read_ok
;           jsr   ToolError
           brl   Fail

read_ok    anop
;            SetStone #"Done with read"     ;debug
           ldx   #$0004                   ;0-4, but 0-5 because of 16-bit
idloop     lda   NuFile,x
           cmp   mhbuf,x
           bne   not_arch
           dex
           dex
           bpl   idloop
           bra   cont

not_arch   anop
           lda   mhbuf
           cmp   #$470a
           bne   notblu
           lda   mhbuf+2
           and   #$00ff
           cmp   #$004c
           bne   notblu
           lda   mhbuf+18
           and   #$00ff
           cmp   #$0002
           bne   notblu
           lda   #buffer                  ;read into general buffer
           sta   pArcRead+$04
           lda   #buffer|-16
           sta   pArcRead+$06
           lda   #128-48                  ;skip rest of BLU header and retry
           jsr   ReadArchive
           bcs   notblu
           bra   retry

notblu     anop
           lda   #eNotArchive
           jsr   MyError
           brl   Fail

cont       anop
; copy interesting data
           lda   mhbuf+$08
           sta   <total_records
           lda   mhbuf+$0a
           sta   <total_records+2

           stz   <crc
           lda   #mhbuf+$08               ;offset to start at
           ldy   #40                      ;48-8
           jsr   CalcCrc

           lda   mhbuf+$06                ;master_crc
           cmp   <crc
           beq   crc_ok
           lda   #eBadMHBlock
           jsr   MyError
           brl   Fail
crc_ok     anop

;
; Now that it's open and looks okay, print something for the user to look at.
;
           lda   <mode
           bit   #dVerbose
           bne   printhdr
           brl   Done                     ;don't print anything
printhdr   bit   #dView
           bne   viewmode

notview    ANOP
; inform the user
           bit   #dStdin                  ;check stdin flag
           bne   use_stdin                ;using stdin, not file

           pea   file1|-16
           pea   file1
           _WriteCString
           pea   archivename|-16
           pea   archivename+2
           _WriteCString
           pea   file2|-16
           pea   file2
           _WriteCString
           brl   Done

use_stdin  anop
           pea   stdin|-16
           pea   stdin
           _WriteCString
           brl   Done


; print the fancy header for view mode
viewmode   ANOP
           ldx   #info
           ldy   #linebuf
           lda   #$0001                   ;copy the null byte too
           jsr   strcpy                   ;copy info line into linebuf
           lda   <mode
           and   #dStdin                  ;check stdin flag
           bne   no_name                  ;no filename to insert...

           ldx   #archivename+2           ;from archivename
           ldy   #linebuf                 ;into linebuf
           lda   #50                      ;at most 50 chars (arbitrary)
           jsr   strncpy                  ;copy it in
           lda   #50
           sec
           sbc   <subtmp                  ;this gives #of chars copied
           tay
           bra   set_type

no_name    anop
           ldx   #stdin_name
           ldy   #linebuf
           lda   #$0000                   ;don't copy the null
           jsr   strcpy                   ;I *know* this is less than 60
           ldy   #7                       ;in fact, it's 7 chars long

; now, overwrite next few chars with the archive type
; (for now, it's always NuFX)
set_type   ANOP
           shortm
           ldx   #$0000
tloop      lda   type,x
           beq   tloop_done               ;don't copy the zero
           sta   linebuf,y
           inx
           iny
           bra   tloop
tloop_done anop
           longm

; insert the #of "items" in the string
           lda   <total_records           ;assume < 65536
           pha
           pea   linebuf|-16
           pea   linebuf+65
           pea   $0005                    ;at most 5 digits
           pea   $0000                    ;unsigned
           _Int2Dec

; now print the lines
           pea   linebuf|-16
           pea   linebuf
           _WriteCString
           pea   header|-16
           pea   header
           _WriteCString


Done       ANOP
;           SetStone #"MHBlock done"       ;debug
           clc
           rts

Fail       ANOP
           sec
           rts

narch      ANOP
           lda   #eNotArchive
           jsr   MyError
           sec
           rts

NuFile     dc    H'4ef546e96ce5'          ;"NuFile" in alternating ASCII
file1      dc    C'Reading ',H'00'
file2      dc    C'...',H'0d0a00'
stdin      dc    C'Reading from stdin...',H'0d0a00'
stdin_name dc    C'(stdin)',H'00'
info       dc    C'                                        ' ;40 spaces
           dc    C'                               Items ',H'0d0a0d0a00'
type       dc    C' (NuFX)',H'00'
header     dc    C'Name                Kind   Typ AuxTyp Archived         '
           dc    C'Format Size Un-Length',H'0d0a'
           dc    C'------------------- ------ --- ------ ---------------- '
           dc    C'------ ---- ---------',H'0d0a00'
           END


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
ProcessRecord START
;
; Calling conventions:
;   JSR
;   Returns with carry set on error.
;   * Expects the file to be positioned at the start of a record header.
;   * On exit, the file is positioned at the start of the next record.
;
; This routine does whatever is appropriate to the next record for the
; current <mode.  It begins by reading the record and thread headers, and
; then calls other routines to extract or ignore the thread data.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
           using globals
           using ParmBlocks

;           SetStone #"into ProcessRecord" ;debug

           lda   #rhbuf                   ;set to record header buffer
           sta   pArcRead+$04
           lda   #rhbuf|-16
           sta   pArcRead+$06
           lda   #$0001                   ;cache header blocks
           sta   pArcRead+$10             ;cachePriority
           lda   #$0008
           jsr   ReadArchive              ;get ID, crc, attrib count

           ldx   #$0002                   ;0-2, but 0-3 because 16-bit
idloop     lda   NuFX,x
           cmp   rhbuf,x
           bne   bad_rec
           dex
           dex
           bpl   idloop
           bra   cont

bad_rec    lda   #eBadRHId
           jsr   MyError
           brl   Fail
cont       anop

; read the rest of the attributes into the main buffer
; (note I'm ignoring all that option_list stuff... may have to fix later)
           lda   #rhbuf+$08               ;advance read buffer
           sta   pArcRead+$04
           lda   rhbuf+$06                ;attrib_count
           cmp   #1025                    ;are we huge?
           blt   ac_ok
           lda   #eHugeRH                 ;record header bigger than buf
           jsr   MyError
           brl   Fail
ac_ok      anop
           sec
           sbc   #$0008                   ;read attrib_count - $08 bytes
           jsr   ReadArchive
           bcc   read2_ok
           jsr   ToolError
           brl   Fail
read2_ok   anop

; check the record version
           lda   rhbuf+$08                ;check version #
           cmp   #$0004                   ;we max out at 3
           blt   vers_ok
           pea   vers_warning|-16
           pea   vers_warning
           _ErrWriteCString
vers_ok    anop

; grab some other stuff
           lda   rhbuf+$0a                ;total_threads
           sta   <total_threads
           lda   rhbuf+$10                ;file_sys_info
           and   #$00ff
           sta   <filename_sep

; okay, now read the filename in (if any)
           ldy   rhbuf+$06                ;attrib_count
           dey                            ;back up two bytes
           dey
           lda   rhbuf,y                  ;get size of filename
           sta   filename_buf             ;set length (or indicate none)
           beq   no_name                  ;name must be in thread

           ldx   #filename_buf+2          ;switch to filename buffer
           stx   pArcRead+$04
           jsr   ReadArchive              ;read filename in
           anop                           ;can't normalize until after CRC
           ldx   filename_buf
           stz   filename_buf+2,x         ;null-terminate it
no_name    anop

; okay, now get the threads
;           SetStone #"filename read, getting threads" ;debug
           lda   rhbuf+$0a                ;total_threads
           beq   no_threads
           cmp   #33                      ;max of 32 threads
           blt   threads_ok
           lda   #eLotsaThreads
           jsr   MyError
           brl   Fail
no_threads anop
           lda   #eNoThreads
           jsr   MyError
           brl   Fail
threads_ok anop
           asl   A                        ;multiply x16
           asl   A
           asl   A
           asl   A
           sta   <tmp                     ;save for CRC calc
           ldx   #thread_buf              ;switch to thread buffer
           stx   pArcRead+$04
           jsr   ReadArchive              ;read the threads in

; compute a CRC on all this stuff
           stz   <crc                     ;init
           lda   rhbuf+$06                ;attrib_count
           sec
           sbc   #$0006                   ;don't crc the crc
           tay
           lda   #rhbuf+$06
           jsr   CalcCrc

           ldy   filename_buf
           beq   no_name2
           lda   #filename_buf+2          ;filename, but not length bytes
           jsr   CalcCrc
           jsr   NormalizePath            ;now we can do this
no_name2   anop

           ldy   <tmp                     ;get size of threads
           lda   #thread_buf
           jsr   CalcCrc

           lda   rhbuf+$04                ;header_crc
           cmp   <crc
           beq   crc_ok
           lda   #eBadRHBlock
           jsr   MyError
           bra   Fail
crc_ok     anop

;
; okay, we've got the record and it looks good.  Go play with threads.
;
           stz   <skip_mode
           ldy   filename_buf
           beq   no_name3
           jsr   MatchName                ;was name given on command line?
           bcs   no_name3                 ;yes
           inc   <skip_mode               ;no, skip data
no_name3   anop
           jsr   ProcessThreads
           bcs   Fail

           lda   <mode
           and   #dView
           beq   Done
           lda   <skip_mode
           bne   Done
           jsr   PrintRecord              ;print a line of info

Done       ANOP
           clc
           rts

Fail       ANOP
           sec
           rts

NuFX       dc    H'4EF546D8'              ;"NuFX" in alternating ASCII
vers_warning dc  C'warning: possibly incompatible record format',H'0d0a00'
           END


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
ProcessThreads PRIVATE
;
; Calling conventions:
;   JSR
;   Returns with carry set on error.
;   * Expects the file to be positioned at the start of the thread data
;   * On exit, the file is positioned immediately after the thread data
;
; This examines the list of threads, and decides on what actions to take
; (extract, print info, skip over, etc).  If we are simply viewing an
; archive, this routine will scan the threads, gather some information on
; them, and then skip over the thread data.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
           using globals
           using ParmBlocks

;
; Pass 1: gather some information on the threads
;
; We need to know, for example, whether the file is to be normal or forked
; before we can create it.  Well, not necessarily - CreateGS can add a
; resource fork to a normal file - but I have a sneaking suspicion that
; it will be more efficient to do it right the first time.  At any rate,
; it isn't much more work for us.
;
           stz   pArcRead+$10             ;cachePriority (don't cache data)

           stz   <rec_kind                ;init to dFile
           stz   <data_unlen              ;init all lengths to zero
           stz   <data_unlen+2
           stz   <rsrc_unlen
           stz   <rsrc_unlen+2
           stz   <total_clen
           stz   <total_clen+2

           lda   <total_threads
           sta   <tmp                     ;use this as a counter
           ldy   #thread_buf              ;init to first thread

loop1      ANOP
           sty   <ptr                     ;for safe-keeping
           lda   |$0000,y                 ;get thread_class
           cmp   #$0002
           beq   data1

           bra   end_of_loop1             ;ignore others for now

data1      anop
           lda   |$0004,y                 ;thread_kind
           beq   vdata
           cmp   #$0002
           beq   vrsrc
           cmp   #$0001
           beq   vdisk
           lda   #dOther
           sta   <rec_kind
           bra   vdata                    ;do the other data stuff
vdisk      anop
           lda   #dDisk
           sta   <rec_kind
           bra   vdata                    ;do all the other data stuff
vrsrc      anop
           lda   #dForked
           sta   <rec_kind                ;indicate presence of resource fork
           lda   <rec_format
           bpl   vdata                    ;already have format
           lda   |$0002,y                 ;thread_format
           sta   <rec_format

           ldy   <ptr
           lda   |$0008,y                 ;thread_eof
           clc
           adc   <rsrc_unlen
           sta   <rsrc_unlen
           lda   |$000a,y                 ;thread_eof+2
           adc   <rsrc_unlen+2
           sta   <rsrc_unlen+2
           bra   vall

vdata      anop
           ldy   <ptr
           lda   |$0008,y                 ;thread_eof
           clc
           adc   <data_unlen
           sta   <data_unlen
           lda   |$000a,y                 ;thread_eof+2
           adc   <data_unlen+2
           sta   <data_unlen+2

vall       anop                           ;(everybody converges here)
           lda   |$000c,y                 ;comp_thread_eof
           clc
           adc   <total_clen
           sta   <total_clen
           lda   |$000e,y                 ;comp_thread_eof+2
           adc   <total_clen+2
           sta   <total_clen+2
           lda   |$0002,y                 ;thread_format
           sta   <rec_format

end_of_loop1 anop
           dec   <tmp                     ;done with all threads?
           beq   done1
           lda   <ptr
           clc
           adc   #$0010                   ;move to next thread header
           tay
           brl   loop1

done1      anop
           lda   <data_unlen
           clc
           adc   <rsrc_unlen
           sta   <total_unlen
           lda   <data_unlen+2
           adc   <rsrc_unlen+2
           sta   <total_unlen+2

;
; Pass 2: get down and dirty
;
; If we're just viewing, then our work is done (unless there's a filename to
; grab), but we still have to skip over the data in each thread.  Might be
; marginally more efficent to add them up and skip it all at once, but since
; it doesn't require any disk access (file), or requires nothing but (stdin),
; it shouldn't make more than a few miliseconds difference.
;

;           stz   <skip_mode              ;(this is now cleared earlier)
           stz   <created                 ;create that thar file
           ldy   #thread_buf              ;back to first thread

loop       ANOP
           sty   <ptr                     ;for safe-keeping
           lda   <skip_mode               ;are we skipping the rest of this
           beq   dont_skip                ; record?
           brl   skip_thread

dont_skip  anop
           lda   |$0000,y                 ;get thread_class
           beq   message_thread
           cmp   #$0001
           beq   control_thread
           cmp   #$0002
           beq   data_thread
           cmp   #$0003
           beq   filename_thread

           pea   unknown_class|-16
           pea   unknown_class
           _ErrWriteCString
           brl   skip_thread

; these are comments or //gs icons.  For now we quietly ignore them.
message_thread ANOP
           brl skip_thread

; the only defined control_thread is "create directory", and nobody uses
; that yet.  I've adopted a general policy of ignoring control_threads.
control_thread ANOP
           pea   cntl_warning|-16
           pea   cntl_warning
           _ErrWriteCString
           brl skip_thread

; this is where filenames get stuck these days.
filename_thread ANOP
           lda   |$0004,y                 ;check thread_kind
           beq   ft_ok
           pea   weird_ft|-16
           pea   weird_ft
           _ErrWriteCString
ft_ok      anop
           lda   |$0008,y                 ;assume filename is <65536 chars
           sta   filename_buf             ;set length word
           lda   #filename_buf+2          ;read this filename
           sta   pArcRead+$04
           lda   #filename_buf|-16
           sta   pArcRead+$06
           lda   |$000c,y                 ;assume whole thing is <= 1K
           jsr   ReadArchive              ;read it in
           bcc   fread_ok
           jsr   ToolError
           brl   Fail
fread_ok   anop
           ldy   <ptr
           lda   |$0008,y                 ;thread_eof
           tay
           lda   #$0000
           sta   filename_buf+2,y         ;null-terminate the sucker

           jsr   NormalizePath            ;clean it up
           jsr   MatchName                ;does it match a specified name?
           bcs   matched
           inc   <skip_mode               ;nope, skip this thread
matched    anop
           brl   end_of_loop

; these are the meat & potatoes
data_thread ANOP
           lda   <mode
           and   #dView                   ;just viewing?
           beq   yankit                   ;nope
           brl   skip_thread

yankit     anop
           lda   filename_buf             ;do we have a non-NULL filename?
           bne   name_exists
           pea   noname_warn|-16          ;no filename, so we don't know where
           pea   noname_warn              ; to put the data.  Wimp out and
           _ErrWriteCString               ; skip the rest.  (could be more
           inc   <skip_mode               ; intelligent here.)
           bra   skip_thread

name_exists anop
           jsr   ExtractData
           bcc   extract_ok
           brl   Fail

extract_ok anop
           lda   <skip_mode               ;aborted extract NOT due to error?
           beq   end_of_loop              ;no, go to end

; skip over thread contents (comp_thread_eof bytes)
skip_thread ANOP
           lda   #buffer                  ;use compression buffer (need 8K)
           sta   pArcRead+$04             ; (only matters for stdin)
           lda   #buffer|-16
           sta   pArcRead+$06
           ldy   <ptr
           lda   |$000e,y                 ;comp_thread_eof
           tax                            ;hi word
           lda   |$000c,y                 ;lo word
           jsr   SkipArchive              ;skip over all the stuff
           bcs   Fail

end_of_loop ANOP
           lda   <abort                   ;abort program?
           bne   loop_done
           dec   <total_threads           ;done with all threads?
           beq   loop_done
           lda   <ptr
           clc
           adc   #$0010                   ;move to next thread header
           tay
           brl   loop
loop_done  anop

; if we created a file, we'd better set the file info
           lda   <created
           beq   Done
           lda   <mode
           and   #dIntegrity
           bne   Done

           lda   rhbuf+$12
           sta   pSetFileInfo+$06
           lda   rhbuf+$16
           sta   pSetFileInfo+$08

           ldx   #$0014
info_loop  lda   rhbuf+$1a,x              ;copy access through mod_when
           sta   pSetFileInfo+$0a,x
           dex
           dex
           bpl   info_loop
           jsr   SetFileInfo              ;ignore errors on this one

Done       ANOP
           clc
           rts

Fail       ANOP
           sec
           rts

unknown_class dc C'warning: encountered thread_class > $0003',H'0d0a00'
cntl_warning dc  C'warning: ignoring control_thread',H'0d0a00'
weird_ft   dc    C'warning: unknown filename thread_kind',H'0d0a00'
noname_warn dc   C'warning: found data before filename, skipping record'
           dc    H'0d0a00'
           END


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
ExtractData PRIVATE
;
; Calling conventions:
;   JSR with the thread_kind in Acc and the thread header offset in <ptr.
;
; This handles the extraction of data and resource forks.
;
; There are a couple of cases where we give the user an option.  If the
; compression format is one we don't understand (like UNIX compress), or
; if the thread isn't part of a normal file (e.g. a disk image), then we
; ask the user if he really wants to go through with the extraction.  If
; so, then (in case 1) we extract it as raw compressed data, or (in case
; 2) we extract it into the data fork of a file.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
           using globals
           using ParmBlocks

; see what kind of compression we've got here
           ldy   <ptr
           lda   |$0002,y                 ;thread_format
           sta   <tmp
           beq   fmt_ok
           cmp   #$0001
           beq   squeezed
           cmp   #$0002
           beq   fmt_ok
           cmp   #$0003
           beq   fmt_ok
           cmp   #$0004
           beq   unix12
           cmp   #$0005
           beq   unix16
           bra   unknown

fmt_ok     brl   fmt_ok2

unknown    anop
           lda   #kindXXXX
           bra   skip_msg
squeezed   anop
           lda   #kind0001
           bra   skip_msg
unix12     anop
           lda   #kind0004
           bra   skip_msg
unix16     anop
           lda   #kind0005

; print a message saying that we're skipping this fork (kinda fancy)
skip_msg   anop
           pha                            ;save msg addr
           pea   msg1|-16
           pea   msg1
           _ErrWriteCString
           lda   <rec_kind                ;was it forked?
           cmp   #dForked
           beq   sm_fork                  ;yes, print which fork
           pea   msg2n|-16                ;no, print generic message
           pea   msg2n
           bra   sm_2done
sm_fork    lda   <which                   ;which fork was that?
           bne   sm_rsrc                  ;was resource fork
           pea   msg2d|-16                ;was data fork
           pea   msg2d
           bra   sm_2done
sm_rsrc    pea   msg2r|-16
           pea   msg2r
sm_2done   anop
           _ErrWriteCString
           pea   msg3|-16
           pea   msg3
           _ErrWriteCString
           pla
           pea   kindXXXX|-16
           pha
           _ErrWriteCString
           pea   msg4|-16
           pea   msg4
           _ErrWriteCString
           pea   fquery|-16               ;ask user if he wants to just pull
           pea   fquery                   ; it out as raw data
           _ErrWriteCString
           lda   #"N"
           jsr   Query
           cmp   #"Y"
           beq   rawyank

           pea   filename_buf|-16
           pea   filename_buf+2
           _ErrWriteCString
           pea   msg5|-16
           pea   msg5
           _ErrWriteCString
           brl   skip_data

rawyank    ANOP
           ldy   <ptr
           lda   #$0000                   ;pretend it's uncompressed
           sta   |$0002,y                 ;thread_format

; okay, the compression format is something we understand
fmt_ok2    anop
           lda   <mode
           and   #dIntegrity
           beq   openit
           brl   open_ok                  ;pretend it's open

; figure out which fork we're supposed to be dealing with here
openit     anop
           ldy   <ptr
           lda   |$0004,y                 ;thread_kind
           sta   <which
           beq   file                     ;data_fork
           cmp   #$0002
           beq   file                     ;rsrc_fork

; it's a disk image or something weird.  Deal with it.
           cmp   #$0001                   ;disk_image
           bne   notd
           pea   idisk|-16
           pea   idisk
           _ErrWriteCString
           bra   cont2
notd       anop
           pea   iunknown|-16
           pea   iunknown
           _ErrWriteCString
cont2      anop
           pea   tquery|-16
           pea   tquery
           _ErrWriteCString
           lda   #"N"
           jsr   Query
           cmp   #"Y"
           beq   justdoit
           pea   filename_buf|-16
           pea   filename_buf+2
           _ErrWriteCString
           pea   msg5|-16
           pea   msg5
           _ErrWriteCString
           bra   skip_data

justdoit   anop
           stz   <which                   ;pretend it's a normal data fork

file       ANOP
; The first time we extract a thread, we need to create the destination
; file and open it for writing.
           lda   <created  
           bne   is_created
           jsr   CreateFile
           bcc   create_ok

           pea   filename_buf|-16
           pea   filename_buf+2
           _ErrWriteCString
           pea   msg5|-16
           pea   msg5
           _ErrWriteCString
           inc   <skip_mode               ;couldn't create it, so skip
           bra   Done                     ; the whole record
create_ok  anop
           inc   <created  
is_created anop

; open the appropriate fork
; (at this point, <which should be 0 or 2 for data or rsrc, respectively)
           lda   <which
           lsr   A                        ;change 0/2 to 0/1
           sta   <which
           jsr   OpenFile
           bcc   open_ok
           jsr   ToolError
           inc   <skip_mode               ;skip this thread then
           bra   Done
open_ok    anop

; finally, the file is created and the appropriate fork is open
           ldy   <ptr
           lda   |$0004,y                 ;thread_kind
           cmp   #$0001                   ;disk_image?
           bne   not_disk
           jsr   FixDisk                  ;yup, fix that thread_eof
not_disk   anop
           lda   |$0002,y                 ;thread_format
           beq   uncompressed

lzw        anop                           ;if not uncompressed, must be lzw
           jsr   ExtractSHK
           bcc   ext_ok
           bra   Fail

uncompressed anop
           jsr   Extract                  ;extract uncompressed file
           bcc   ext_ok
           bra   Fail

skip_data  ANOP
           lda   #buffer                  ;use compression buffer (need 8K)
           sta   pArcRead+$04             ; (only matters for stdin)
           lda   #buffer|-16
           sta   pArcRead+$06
           ldy   <ptr
           lda   |$000e,y                 ;comp_thread_eof
           tax                            ;hi word
           lda   |$000c,y                 ;lo word
           jsr   SkipArchive              ;skip over all the stuff
           bcs   Fail

ext_ok     ANOP

Done       ANOP
           jsr   CloseFile
           clc
           rts

Fail       ANOP
           jsr   CloseFile                ;safe even if file not open
           sec
           rts

msg1       dc    C'warning: found ',H'00'
msg2n      dc    C'thread',H'00'
msg2d      dc    C'data fork',H'00'
msg2r      dc    C'resource fork',H'00'
msg3       dc    C' stored in ',H'00'
kindXXXX   dc    C'an unknown',H'00'
kind0001   dc    C'squeezed',H'00'
kind0004   dc    C'UNIX compress',H'00'
kind0005   dc    C'UNIX compress',H'00'
msg4       dc    C' format',H'0d0a00'
idisk      dc    C'warning: found a disk image',H'0d0a00'
iunknown   dc    C'warning: found a bizarre thread',H'0d0a00'
fquery     dc    C'Extract as raw data',H'00'
tquery     dc    C'Extract as data fork of normal file',H'00'
msg5       dc    C' not extracted',H'0d0a00'
           END


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
FixDisk    START
;
; Calling conventions:
;   JSR with the thread header pointed to by <ptr
;
; This adjusts the thread's thread_eof to be something useful.  Here's the
; code from NuLib:
;
; /* thread_eof is invalid for disks, so figure it out */
; old_eof = TNodePtr->THptr->thread_eof;
; if (RNodePtr->RHptr->storage_type <= 3) {       /* should be block */
;     TNodePtr->THptr->thread_eof =               /* size, but shk301 */
;         RNodePtr->RHptr->extra_type * 512;      /* stored it wrong */
; } else {
;     TNodePtr->THptr->thread_eof =
;         RNodePtr->RHptr->extra_type * RNodePtr->RHptr->storage_type;
; }
;
; While it's here, it sets the thread's access bits to something more
; friendly, and clears the aux_type to zero.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
           using globals

           pea   $0000                    ;space for result
           pea   $0000
           lda   rhbuf+$1a                ;aux_type == total #of blocks
           pha
           lda   rhbuf+$1e                ;storage_type
           cmp   #4
           bge   size_ok
           lda   #512                     ;assume ProDOS block size
size_ok    anop
           pha
           _Multiply
           ldy   <ptr
           pla
           sta   |$0008,y                 ;thread_eof low
           pla
           sta   |$000a,y                 ;thread_eof high

           lda   #$00c3
           sta   rhbuf+$12                ;access
           stz   rhbuf+$1a                ;zero out the aux type
           stz   rhbuf+$1c
           rts
           END


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
PrintRecord PRIVATE
;
; Calling conventions:
;   JSR with the thread header offset in <ptr (expects th_kinds and
;     total_unlen to be set up as well)
;
; This prints a line in ShrinkIt v3.2 format (verbose mode), or the full
; filename.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
           using globals
dMaxFname  equ   19                       ;filename field width

; if we're not in verbose mode, just dump a list of filenames
           lda   <mode
           and   #dVerbose
           bne   verbose

           pea   filename_buf|-16
           pea   filename_buf+2
           _WriteCString
           pea   crlf|-16
           pea   crlf
           _WriteCString                  ;why don't we have WriteCLine?!?
           brl   Done

verbose    ANOP
; first, erase linebuf
           lda   #$0000
           sta   linebuf+80
           lda   #$0d0a
           sta   linebuf+78
           ldy   #76                      ;erase first 78 chars
           lda   #$2020
clearloop  sta   linebuf,y
           dey
           dey
           bpl   clearloop

; next, copy part or all of the filename in
           lda   filename_buf
           cmp   #dMaxFname               ;too big for all to fit?
           beq   smallname
           bge   bigname

smallname  anop
           lda   #$0000                   ;don't copy null
           ldx   #filename_buf+2
           ldy   #linebuf
           jsr   strcpy
           bra   namedone

bigname    anop
           lda   #".."
           sta   linebuf                  ;stick a couple of dots at front
           lda   filename_buf             ;get length
           sec
           sbc   #dMaxFname-2             ;print dMaxFname-2 chars
           clc
           adc   #filename_buf+2          ;this is start posn
           tax
           ldy   #linebuf+2
           lda   #$0000                   ;don't copy null
           jsr   strcpy
namedone   anop
           lda   #linebuf+dMaxFname+1
           sta   <ptr2

; now do "kind"
           lda   <rec_kind
           asl   A
           asl   A
           asl   A
           tax
           ldy   #$0000
kloop      lda   kinds,x
           sta   (<ptr2),y
           inx
           inx
           iny
           iny
           cpy   #$0006
           blt   kloop

           lda   <ptr2
           clc
           adc   #$0007
           sta   <ptr2

; next is "type"
           lda   rhbuf+$0e                ;file_sys_id
           cmp   #$0001                   ;ProDOS/SOS
           beq   t_prodos
           ldx   #1024                    ;not ProDOS (256)*4 == "---"
           bra   t_copy                   ; (may also hold for disks)

t_prodos   anop
           lda   <rec_kind
           cmp   #dFile
           beq   t_normal
           cmp   #dForked
           beq   t_normal
           ldx   #1024                    ;(256)*4 == "---"
           bra   t_copy

t_normal   anop
           lda   rhbuf+$18                ;file_type+2
           bne   t_weird
           lda   rhbuf+$16
           cmp   #$100
           bge   t_weird
           asl   A
           asl   A
           tax
           bra   t_copy

t_weird    anop
           ldx   #1032                    ;(257)*4 == "???"

t_copy     anop
           lda   types,x                  ;get first two chars
           sta   (<ptr2)
           lda   types+2,x                ;get last char + space
           ldy   #$0002
           sta   (<ptr2),y

           lda   <ptr2
           clc
           adc   #$0004
           sta   <ptr2

; okay, now do the auxtype
           lda   <rec_kind
           cmp   #dDisk
           beq   disk_aux
           lda   rhbuf+$0e                ;file_sys_id
           cmp   #$0001                   ;ProDOS/SOS
           beq   aux_prodos

no_aux     anop
           ldx   #no_auxtype
           ldy   <ptr2
           lda   #$0000
           jsr   strcpy
           bra   aux_done

disk_aux   anop
           lda   #" k"                    ;disk aux type is disk size in K
           ldy   #$0005
           sta   (<ptr2),y
           pea   $0000                    ;push space for result
           pea   $0000
           lda   rhbuf+$1a                ;aux_type == #of blocks
           pha
           lda   rhbuf+$1e                ;storage_type == block size
           cmp   #4
           bge   size_ok
           lda   #512                     ;assume ProDOS block size
size_ok    anop
           pha
           _Multiply
           pla                            ;pull low
           sta   <tmp
           pla                            ;pull hi

           asl   <tmp                     ;divide by 1024 to get K
           rol   A                        ; (either shift right 10 times and
           asl   <tmp                     ; keep low, or shift left 6 times
           rol   A                        ; and keep hi)
           asl   <tmp
           rol   A
           asl   <tmp
           rol   A
           asl   <tmp
           rol   A
           asl   <tmp
           rol   A
           pha                            ;convert this value
           pea   linebuf|-16
           lda   <ptr2
           pha
           pea   $0005
           pea   $0000
           _Int2Dec
; calling FixDisk will set the thread_eof to something meaningful, and then
; wipe the file type and aux type fields (it's main purpose is to set the
; data so that the normal file save routines can process it).  However, we
; no longer need these values for this thread (and this'll give us a nice
; thread_eof to compute a % on).
;
; Whoops.  It's setting the thread_eof in the thread header, not the DP
; total uncomp len.  Making it work right could have side effects.  I'd
; rather just see garbage values.
;
;           jsr   FixDisk
           bra   aux_done

aux_prodos anop
           lda   #"$ "                    ;(it comes out backward)
           sta   (<ptr2)
           lda   rhbuf+$1a                ;aux_type
           pha
           pea   linebuf|-16
           lda   <ptr2
           inc   A
           inc   A
           pha
           pea   $0004
           _Int2Hex

aux_done   anop
           lda   <ptr2
           clc
           adc   #$0007
           sta   <ptr2

; we're cruising.  Print the archive_when field
           lda   rhbuf+$34
           pha
           lda   rhbuf+$32
           pha
           lda   rhbuf+$30
           pha
           lda   #linebuf|-16
           pha
           lda   <ptr2
           pha
           jsr   AsciiDate

           lda   <ptr2
           clc
           adc   #$0011
           sta   <ptr2

; stuff in the format (of the data fork, or the rsrc fork if none)
           lda   <rec_format
           cmp   #$0006
           blt   f_ok
           lda   #$0006
           bra   f2
f_ok       anop
f2         asl   A
           asl   A
           asl   A
           tax
           ldy   #$0000
f_loop     lda   formats,x
           sta   (<ptr2),y
           inx
           inx
           iny
           iny
           cpy   #$0006
           blt   f_loop

           lda   <ptr2
           clc
           adc   #$0007
           sta   <ptr2

; now for a fun one, the percentage of compression
           lda   <total_unlen
           ora   <total_unlen+2
           beq   p_no_unlen

           lda   <total_clen
           ora   <total_clen+2
           bne   p_len_ok

; at this point, we know that total_unlen is non-zero, or we wouldn't be here.
; thus, this file is weird.
p_weird    anop
           lda   #"- "                    ;put " ---" in space
           sta   (<ptr2)
           ldy   #$0002
           lda   #"--"
           sta   (<ptr2),y
           brl   p_done

p_no_unlen anop
           lda   <total_clen              ;see if this is zero too
           ora   <total_clen+2
           beq   p_zero                   ;file is empty
           bra   p_weird                  ;one is zero, one isn't... weird

p_zero     anop
           lda   #"01"
           sta   (<ptr2)
           lda   #"%0"                    ;put "100%" in space
           ldy   #$0002
           sta   (<ptr2),y
           brl   p_done

p_negative anop
           lda   #"< "                    ;put " <0%" in space
           sta   (<ptr2)
           lda   #"%0"
           ldy   #$0002
           sta   (<ptr2),y
           bra   p_done

p_len_ok   anop
           lda   <total_unlen+2
           cmp   <total_clen+2            ;make sure we actually got smaller
           beq   p_checklow
           bge   p_len_ok2
           bra   p_negative               ;uncompressed < compressed!
p_checklow lda   <total_unlen             ;hi was equal, check low
           cmp   <total_clen
           bge   p_len_ok2
           bra   p_negative               ;oh dear

; finally, we've established that the sizes are legitimate
p_len_ok2  anop
           pea   $0000                    ;64-bit result (caramba!)
           pea   $0000
           pea   $0000
           pea   $0000
           pei   <total_clen+2
           pei   <total_clen
           pea   $0000
           pea   100                      ;multiply comp_thread_eof * 100
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
           pei   <total_unlen+2           ;divide by thread_eof
           pei   <total_unlen
           _LongDivide
           pla                            ;this is the quotient (lo)
           bne   p_gtzero                 ;display kludge: don't show 0%
           inc   A
p_gtzero   anop
           ply                            ;quotient (hi)
           ply                            ;remainder
           ply
           pha                            ;push back on for _Int2Dec
           pea   linebuf|-16
           pei   <ptr2
           pea   $0003                    ;at most 3 digits
           pea   $0000
           _Int2Dec

           shortm
           ldy   #$0001
           lda   (<ptr2),y                ;check for space; make 5% == 05%
           cmp   #" "
           bne   p_nospc
           lda   #"0"
           sta   (<ptr2),y
p_nospc    anop
           lda   #"%"                     ;the coup de grace
           ldy   #$0003
           sta   (<ptr2),y
           longm

p_done     anop
           lda   <ptr2
           clc
           adc   #$0005
           sta   <ptr2

; for my last trick, the uncompressed length
           lda   <total_unlen
           ora   <total_unlen+2
           bne   u_unlen_ok
           lda   <total_clen              ;clen but no unlen?
           ora   <total_clen+2
           beq   u_unlen_ok               ;no prob, whole thing is zero

           lda   #"??"
           ldy   #$0005
           sta   (<ptr2),y
           ldy   #$0007
           sta   (<ptr2),y
           bra   u_done

u_unlen_ok anop
           pei   <total_unlen+2
           pei   <total_unlen
           pea   linebuf|-16
           pei   <ptr2
           pea   $0009
           pea   $0000
           _Long2Dec

u_done     anop

;
; print the line and bail
;
           pea   linebuf|-16
           pea   linebuf
           _WriteCString

Done       ANOP
;           SetStone #"leaving PrintRecord"    ;debug
           clc
           rts

kinds      dc    C'File    Disk    Forked  Other   '
formats    dc    C'Uncomp  SQueez  LZW-I   LZW-II  Unix12  Unix16  ??????  '
types      dc    C'NON BAD PCD PTX TXT PDA BIN FNT '
           dc    C'FOT BA3 DA3 WPF SOS $0D $0E DIR '
           dc    C'RPD RPI AFD AFM AFR SCL PFS $17 '
           dc    C'$18 ADB AWP ASP $1C $1D $1E $1F '
           dc    C'TDM $21 $22 $23 $24 $25 $26 $27 '
           dc    C'$28 $29 8SC 8OB 8IC 8LD P8C $2F '
           dc    C'$30 $31 $32 $33 $34 $35 $36 $37 '
           dc    C'$38 $39 $3A $3B $3C $3D $3E $3F '
           dc    C'DIC $41 FTD $43 $44 $45 $46 $47 '
           dc    C'$48 $49 $4A $4B $4C $4D $4E $4F '
           dc    C'GWP GSS GDB DRW GDP HMD EDU STN ' ;$53??
           dc    C'HLP COM CFG ANM MUM ENT DVU FIN '
           dc    C'$60 $61 $62 $63 $64 $65 $66 $67 '
           dc    C'$68 $69 $6A BIO $6C TDR PRE HDV '
           dc    C'$70 $71 $72 $73 $74 $75 $76 $77 '
           dc    C'$78 $79 $7A $7B $7C $7D $7E $7F '
           dc    C'$80 $81 $82 $83 $84 $85 $86 $87 '
           dc    C'$88 $89 $8A $8B $8C $8D $8E $8F '
           dc    C'$90 $91 $92 $93 $94 $95 $96 $97 '
           dc    C'$98 $99 $9A $9B $9C $9D $9E $9F '
           dc    C'WP  $A1 $A2 $A3 $A4 $A5 $A6 $A7 '
           dc    C'$A8 $A9 $AA GSB TDF BDF $AE $AF '
           dc    C'SRC OBJ LIB S16 RTL EXE PIF TIF '
           dc    C'NDA CDA TOL DRV LDF FST $BE DOC '
           dc    C'PNT PIC ANI PAL $C4 OOG SCR CDV '
           dc    C'FON FND ICN $CB $CC $CD $CE $CF '
           dc    C'$D0 $D1 $D2 $D3 $D4 MUS INS MDI '
           dc    C'SND $D9 $DA DBM $DC DDD $DE $DF '
           dc    C'LBR $E1 ATK $E3 $E4 $E5 $E6 $E7 '
           dc    C'$E8 $E9 $EA $EB $EC $ED R16 PAS '
           dc    C'CMD $F1 $F2 $F3 $F4 $F5 $F6 $F7 '
           dc    C'$F8 OS  INT IVR BAS VAR REL SYS '
           dc    C'--- ??? '
no_auxtype dc    C'------',H'00'
crlf       dc    H'0d0a00'
           END


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
CalcCRC    START
;
; Calling conventions:
;   JSR with the offset into the current bank in Acc, #of bytes to CRC in
;     the Y-reg, and the initial crc value in <crc
;   The result is left in <crc
;
; Computes a CRC on a range of bytes in the current bank.  Don't call
; this for count=zero bytes.
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
           eor   >crchi,x                 ;add it to the proper table entry
           sta   <crc+1                   ;save it

           lda   >crclo,x                 ;get new lo byte
           sta   <crc                     ;save it back

           iny                            ;iny/cpy takes 6 cycles, +1 for
           cpy   <subtmp                  ; index crossing page boundary
           blt   loop                     ;can't just dec/bne because of shortm

           longm
           rts
           END


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
ArchiveMem START
;
; Static tables and memory buffers for general archive manipulation.  This
; is a code segment to keep things from crossing bank boundaries.
;
; Fortunately these are all zero-filled, so they don't actually occupy
; any space on disk.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
dMaxFargs  equ   32                       ;also in "globals"

;
; buffer used for wildcard matching
;
wfilename  ENTRY
           ds    1024+2+2                 ;1K name +2 length +2 null

;
; Archive filename
;
archivename ENTRY
           ds    1024+2+2                 ;1K name +2 length +2 null

;
; list of filenames to extract (4 byte pointers)
;
fargv      ENTRY
           ds    4*32

;
; Archive master header
;
mhbuf      ENTRY
           ds    48
;
; Record header buffer; if it's bigger than 1K, something is really wrong.
; (note this doesn't include the filename)
;
rhbuf      ENTRY
           ds    1024
;
; File name buffer (pulled from record header or out of a thread)
; (one for the filename, one for the output of ExpandPathGS)
;
filename_buf ENTRY
           ds    1024+2+2                 ;1K name +2 length +2 null
expfilename ENTRY
           ds    1024+4+2                 ;1K name +4 size/len +2 null
;
; Thread buffer; room for 32 (16 bytes each).
;
thread_buf ENTRY
           ds    512
;
; misc buffers
;
linebuf    ENTRY
           ds    128                      ;used when building up a string

           END

