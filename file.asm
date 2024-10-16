;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; file.asm - GS/OS related stuff (reading files, fixing pathnames, etc)
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

           MLOAD YankIt.macros
           MLOAD file.macros


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
Equates    DATA
;
; Read the 2/ainclude/e16.gsos in for this code.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

           COPY  2/ainclude/e16.gsos
           END


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
CheckVersion START
;
; Calling conventions:
;   JSR
;
; Issues a GetVersionGS call to check the GS/OS version against
; dMinVersion.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
           using globals
           using ParmBlocks

           _GetVersionGS pGetVersion
           lda   pGetVersion+$02
           cmp   #dMinVersion
           blt   Fail

Done       ANOP
           clc
           rts

Fail       ANOP
           lda   #eBadVersion
           jsr   MyError
           sec
           rts
           END


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
BeginSession START
;
; Calling conventions:
;   JSR
;
; Issues a BeginSession call.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
           using ParmBlocks

           _BeginSessionGS pSession
           bcc   Done
           jsr   ToolError

Done       ANOP
           rts
           END

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
EndSession START
;
; Calling conventions:
;   JSR
;
; Issues an EndSession call.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
           using ParmBlocks

           _EndSessionGS pSession
           bcc   Done
           jsr   ToolError

Done       ANOP
           rts
           END


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
OpenConsole START
;
; Calling conventions:
;   JSR
;   Returns with carry set on error.
;
; Opens .CONSOLE, and gets its device number.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
           using globals
           using ParmBlocks

           _OpenGS pConOpen
           bcs   Fail
           lda   pConOpen+$02
           sta   pConRead+$02
           sta   pConClose+$02

           _GetDevNumberGS pGetDevNum
           bcs   Fail
           lda   pGetDevNum+$06
;           sta   <dotconsole
           sta   pDStatus+$02
           sta   pDControl+$02

           _DStatusGS pDStatus            ;get current status
           bcs   Fail
           lda   #$8000
           sta   controllist
           _DControlGS pDControl          ;set raw mode
           bcs   Fail

Done       ANOP
           clc
           rts

Fail       ANOP
           pha
           pea   copbad|-16
           pea   copbad
           _ErrWriteCString
           pla
           jsr   ToolError
           sec
           rts
copbad     dc    C'warning: unable to find .CONSOLE: ',H'00'
           END

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
ReadConsole START
;
; Calling conventions:
;   JSR
;   Returns with carry set on error.
;
; Reads from .CONSOLE.  Since we are in raw mode, we will get exactly
; one character, barring some disaster.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
           using ParmBlocks

           _ReadGS pConRead
           bcs   Fail
           lda   pConRead+$08
           cmp   pConRead+$0c
           beq   Done
           pea   badconrd|-16
           pea   badconrd
           _WriteCString
           bra   Fail2

Done       ANOP
           clc
           rts
Fail       ANOP
           jsr   ToolError
Fail2      anop
           sec
           rts
badconrd   dc    C'YankIt: unable to get full read from .CONSOLE',H'0d0a00'
           END

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
CloseConsole START
;
; Calling conventions:
;   JSR
;   Returns with carry set on error.
;
; Closes .CONSOLE.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
           using ParmBlocks

           lda   statuslist               ;copy raw/user-input mode flag
           sta   controllist
           _DControlGS pDControl
           bcs   Fail
           _CloseGS pConClose
           bcs   Fail

Done       ANOP
           clc
           rts
Fail       ANOP
           jsr   ToolError
           sec
           rts
           END


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
OpenArchive START
;
; Calling conventions:
;   JSR
;   Returns with carry set on error.
;
; Opens the archive whose name is stored in "archivename".
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
           using globals
           using ParmBlocks

           stz   <file_posn               ;we're at logical start of file
           stz   <file_posn+2

;           lda   pArcOpen                 ;debug
;           jsl   PrintNum                 ;debug
;           lda   pArcOpen+2               ;debug
;           jsl   PrintNum                 ;debug
;           lda   pArcOpen+4               ;debug
;           ldx   pArcOpen+6               ;debug
;           jsl   PrintLong                ;debug
;           lda   pArcOpen+8               ;debug
;           jsl   PrintNum

           lda   <mode
           and   #dStdin                  ;check stdin flag
           beq   open_file                ;using stdin, not file

use_stdin  ANOP
; nothing to do
           bra   Done

open_file  ANOP
           _OpenGS pArcOpen
           bcs   Fail

           lda   pArcOpen+$02             ;get refNum
           sta   pArcRead+$02             ;store it in the Read block
           sta   pArcSetMark+$02          ;and in SetMark
           sta   pArcClose+$02            ;and in Close

; init the other parm blocks (so we can be restarted from memory)
           lda   #$ffff
           sta   pOpen+$02
           sta   pWrite+$02
           sta   pClose+$02

           bra   Done

Done       ANOP
           clc
           rts

Fail       ANOP
           jsr   ToolError                ;print error message
           sec
           rts

           END

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
ReadArchive START
;
; Calling conventions:
;   JSR with the #of bytes to read in Acc
;   Returns with carry set on failure
;
; This is the only way to read data.  Since we want to be able to
; get input from stdin, we cannot seek randomly or even back up.
; Performance will be shot to hell unless GS/OS is buffering us, which
; fortunately it is (just buffering the 512 byte block is help enough).
;
; DP USAGE:
;   subtmp
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
           using globals
           using ParmBlocks

           sta   <subtmp
           lda   <mode
           and   #dStdin
           bne   from_stdin

from_file  anop
           lda   <subtmp
           sta   pArcRead+$08             ;requestCount
           stz   pArcRead+$0a
           _ReadGS pArcRead
;           bcs   Fail
           bcc   _ok
           brl   Fail
_ok        anop
           lda   pArcRead+$08             ;compare with transferCount
           cmp   pArcRead+$0c
           beq   Done
           lda   #$004c                   ;hit eof early, I guess
           bra   Fail                     ; ($004c is eofEncountered)

from_stdin ANOP
           lda   pArcRead+$06             ;buffer (hi)
           pha
           lda   pArcRead+$04             ;buffer (lo)
           pha
           pea   $0000                    ;offset
           lda   <subtmp
           pha                            ;transfer count
           pea   $0000                    ;don't echo!
           _TextReadBlock                 ;hope this works...
           bcs   Fail

Done       ANOP
           lda   <file_posn               ;advance the file pointer
           clc
           adc   <subtmp
           sta   <file_posn
           lda   <file_posn+2
           adc   #$0000
           sta   <file_posn+2

           clc
           rts

Fail       sec
           jsr   ToolError
           rts
           END


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
SkipArchive START
;
; Calling conventions:
;   JSR with the #of bytes to read in Acc and X-reg (lo, hi)
;   Returns with carry set on failure
;
; This skips past a piece of the file.  If we're reading from a file, we
; can just use SetMarkGS and get it over with quickly.  If we're reading
; from stdin, we have to read all of the intervening data.  Since there
; could be a whole lot of it, we do reads in 8K chunks (it uses whatever
; buffer the ReadGS parm block is pointing at, so make sure that's set
; to a buffer with at least 8K of free space).
;
; DP USAGE:
;   subtmp, subtmp2
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
           using globals
           using ParmBlocks

           sta   <subtmp
           stx   <subtmp2
           ora   <subtmp2
           bne   gtzero                   ;skip 0 bytes?
           brl   Done                     ;(happens on empty files)

gtzero     anop
           lda   <mode
           and   #dStdin
           bne   from_stdin

; if we're reading from a file, we can cheat and just do a seek
from_file  ANOP
           lda   <file_posn               ;advance the file pointer
           clc
           adc   <subtmp
           sta   <file_posn
           lda   <file_posn+2
           adc   <subtmp2
           sta   <file_posn+2

           lda   #$0002                   ;base = old mark (i.e., advance us)
           sta   pArcSetMark+$04
           lda   <subtmp
           sta   pArcSetMark+$06
           lda   <subtmp2
           sta   pArcSetMark+$08
           _SetMarkGS pArcSetMark
           bcc   set_ok
           brl   Fail
set_ok     brl   Done

; from stdin, we have to do things the hard way
from_stdin ANOP
           lda   <file_posn               ;advance the file pointer
           clc
           adc   <subtmp
           sta   <file_posn
           lda   <file_posn+2
           adc   <subtmp2
           sta   <file_posn+2

loop       anop
           lda   <subtmp2
           bne   huge
           lda   <subtmp
           cmp   #8193
           bge   huge

; request is < 8K, so do it all
           tay
           stz   <subtmp                  ;zero request
           bra   do_read

huge       anop
           ldy   #8192                    ;request count
           lda   <subtmp                  ;update subtmp
           sec
           sbc   #8192
           sta   <subtmp
           lda   <subtmp2
           sbc   #$0000
           sta   <subtmp2

do_read    anop
           lda   pArcRead+$06             ;buffer (hi)
           pha
           lda   pArcRead+$04             ;buffer (lo)
           pha
           pea   $0000                    ;offset
           phy                            ;transfer count
           pea   $0000                    ;don't echo!
           _TextReadBlock
           bcs   Fail

read_ok    anop
           lda   <subtmp
           ora   <subtmp2
           bne   loop

Done       ANOP
           clc
           rts

Fail       ANOP
           jsr   ToolError
Fail2      sec
           rts
           END


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
CloseArchive START
;
; Calling conventions:
;   JSR
;   Returns with carry set on error.
;
; Closes the archive file.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
           using globals
           using ParmBlocks

           lda   <mode
           and   #dStdin
           bne   Done                     ;was reading from stdin

           _CloseGS pArcClose             ;close the archive file
           bcs   Fail

Done       ANOP
           clc
           rts

Fail       ANOP
           jsr   ToolError
           sec
           rts
           END


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
NormalizePath START
;
; Calling conventions:
;   JSR with the pathname in filename_buf, and the filename separator
;     char in <filename_sep (low byte)
;
; This changes all pathname separators (e.g. UNIX "/", MS-DOS "\") to
; the world-famous GS/OS colon.  It converts all colons to periods.
; Someday it may try to do rudimentary clean-up operations based on the
; destination file system, but don't hold your breath waiting.  Running
; this twice on the same string would probably be a bad thing to do.
;
; If filename_buf isn't NULL-terminated, this routine could have an
; accident on portions of memory.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
           using globals

           shortm
           ldy   #$0000

loop       anop
           lda   filename_buf+2,y
           beq   Done
           cmp   <filename_sep            ;this could be ':', so we want to
           beq   pathsep                  ; check it first
           cmp   #":"
           bne   cont

           lda   #"."                     ;change ':' to '.'
           sta   filename_buf+2,y

pathsep    anop
           lda   #":"                     ;change '/' (or whatever) to ':'
           sta   filename_buf+2,y         ;(may change ':' to ':' too)
           bra   cont

cont       anop
           iny
           bra   loop

Done       ANOP
           longm
           rts
           END


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
CreateFile START
;
; Calling conventions:
;   JSR with the name of the file to create in filename_buf
;   <rec_kind should hold the type of file (dFile, dForked, dDisk, dOther)
;   Returns with carry set on error.
;
; Creates a file to hold the data we are extracting.  Initially I used
; the GS/OS preallocation feature with <data_unlen and <rsrc_unlen, but
; that prevents us from taking advantage of sparse files.
;
; ***  This will need some fairly sophisticated error handling in the
;      future; this is where bogus filenames and subdirectory creation
;      will be handled.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
           using globals
           using ParmBlocks

           stz   <fnloop                  ;this tries to detect endless loops

           lda   #1024                    ;reset buffer size for expfilename
           sta   expfilename
           ldy   #$0001                   ;standard file
           lda   <rec_kind
           cmp   #dForked
           bne   notforked
           ldy   #$0005                   ;extended file
notforked  anop
           sty   pCreate+$0e              ;storageType

;           lda   <data_unlen
;           sta   pCreate+$10              ;eof
;           lda   <data_unlen+2
;           sta   pCreate+$12
;           lda   <rsrc_unlen
;           sta   pCreate+$14              ;resourceEof
;           lda   <rsrc_unlen+2
;           sta   pCreate+$16

Retry      ANOP
           inc   <fnloop
           lda   <fnloop
           cmp   #16
           blt   not_locked
           lda   #eFileCLoop              ;I know, it ought to go to MyError
           brl   Fail                     ; not ToolError...

not_locked ANOP
           _CreateGS pCreate
           bcs   _err
           brl   Done
_err       anop

; oh dear, an error
           cmp   #dupPathName             ;$0047
           beq   is47
           brl   not47

; the file already exists, so nuke it if the user so desires
is47       anop
           _ExpandPathGS pExpandPath
           bcc   ok
           brl   Fail
ok         anop
           ldy   expfilename+2
           lda   #$0000
           sta   expfilename+4,y

           lda   <mode                    ;force mode active?
           bit   #dForce
           beq   noforce
           bit   #dVerbose
           beq   destroy
           pea   overwriting|-16
           pea   overwriting
           _WriteCString
           pea   expfilename|-16
           pea   expfilename+4
           _WriteCString
           pea   crlf|-16
           pea   crlf
           _WriteCString
           bra   destroy

noforce    anop
           pea   overwrite|-16
           pea   overwrite
           _ErrWriteCString
           pea   expfilename|-16
           pea   expfilename+4
           _ErrWriteCString
           lda   #"N"                     ;default is don't overwrite
           jsr   Query                    ; (least destructive path)
           cmp   #"Y"
           bne   Fail2                    ;'N' or 'Q'
destroy    anop
           _DestroyGS pDestroy            ;nuke the interfering %$#*@!
           bcs   Fail
           brl   Retry

not47      ANOP
           cmp   #pathNotFound            ;$0044
           bne   not44

; looks like we've got stuff in a subdirectory.  Try to create it.
           _ExpandPathGS pExpandPath
           bcs   Fail
           jsr   CreatePath
           bcs   Fail2
           brl   Retry

not44      ANOP
;           cmp   #badPathSyntax           ;$0040

; didn't match anything we know, so we have failed (sigh)
Fail       ANOP
           pha
           pea   nocreatemsg|-16
           pea   nocreatemsg
           _ErrWriteCString
           pla
           jsr   ToolError
Fail2      anop
           sec
           rts

Done       ANOP
           clc
           rts

overwrite  dc    C'Overwrite ',H'00'
overwriting dc   C'Overwriting ',H'00'
nocreatemsg dc   C'warning: unable to create file' ;(and fall into...)
crlf       dc    H'0d0a00'
           END


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
CreatePath START
;
; Calling conventions:
;   JSR
;
; Creates the subdirectories which don't exist in the path given in
; expfilename.  Uses "wfilename" as temporary space while building the
; path.
;
; DP USAGE:
;  fnptr, fnptr2, subtmp, subtmp2
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
           using globals
           using ParmBlocks

;           SetStone #"In CreatePath"      ;debug

           stz   <fnptr
           stz   <fnptr3
           stz   wfilename

loop       ANOP
           shortm
           ldy   <fnptr

find_break anop
           lda   filename_buf+2,y
           beq   found_end
           cmp   #":"                     ;look for a path sep
           beq   found_break
           iny
           bra   find_break

found_break anop
found_end  anop
           sty   <fnptr2
           longm                          ;only really need to do this if
           tya                            ; path components are > 256 bytes
           sec
           sbc   <fnptr
           sta   <subtmp                  ;counter
           sta   <subtmp2                 ;save for later

           shortm
           ldy   <fnptr
           ldx   <fnptr3
copy       anop
           lda   filename_buf+2,y
           sta   wfilename+2,x
           iny
           inx
           dec   <subtmp
           bpl   copy                     ;get the zero too

; update the new pathname
           longm
           lda   <fnptr3
           clc
           adc   <subtmp2                 ;(if we fix the filename, then we
           sta   <fnptr3                  ; add the NEW length here)
           lda   wfilename
           clc
           adc   <subtmp2                 ;increase the length word
           sta   wfilename

;           shortm                         ;debug - NULL-terminate the partial
;           lda   #$00                     ;debug   pathname
;           ldx   <fnptr3                  ;debug
;           sta   wfilename+2,x            ;debug
;           longm                          ;debug

; check to see if we've hit the last component
           shortm
           ldy   <fnptr2
           lda   filename_buf+2,y
           bne   createit
           bra   loop_done

; now we've got the next component of the filename transferred over to
; wfilename.  Try to create it.
createit   anop
           longm
;           writestr #"creating "          ;debug
;           pea   wfilename|-16            ;debug
;           pea   wfilename+2              ;debug
;           _WriteCString                  ;debug
;           pea   crlf|-16                 ;debug
;           pea   crlf                     ;debug
;           _WriteCString                  ;debug

           _CreateGS pCreateDir
           bcc   create_ok
           cmp   #dupPathname             ;$0047 - file already exists?
           beq   create_ok                ; then there's no need to create it
           jsr   ToolError
           brl   Fail

create_ok  anop

; not done, so add an fssep to the output string
           ldy   <fnptr3
           lda   #"::"
           sta   wfilename+2,y
           iny
           sty   <fnptr3
           inc   wfilename                ;update length

           ldy   <fnptr2                  ;old end +1 becomes new start
           iny
           sty   <fnptr
           brl   loop

loop_done  ANOP
           longm
           lda   #$0000                   ;NULL-terminate the final path
           ldx   <fnptr3
           sta   wfilename+2,x

           lda   wfilename
           clc
           adc   #$0004                   ;catch length word and nulls
           tay
copyback   anop
           lda   wfilename,y
           sta   filename_buf,y
           dey
           bpl   copyback

Done       ANOP
           longm
;           SetStone #"Leaving CreatePath" ;debug
;           writestr #"final path: "       ;debug
;           pea   filename_buf|-16         ;debug
;           pea   filename_buf+2           ;debug
;           _WriteCString                  ;debug
;           pea   crlf|-16                 ;debug
;           pea   crlf                     ;debug
;           _WriteCString                  ;debug
           clc
           rts

Fail       ANOP
           longm
;           SetStone #"Failed in CreatePath" ;debug
           sec
           rts

;crlf       dc    H'0d0a00'                ;debug
           END


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
OpenFile   START
;
; Calling conventions:
;   JSR with the name of the file to open in filename_buf, and either
;     $0000 in Acc for data fork or $0001 in Acc for resource fork
;
; Opens a destination file (i.e., one that we are going to extract into).
; Sets up the WriteFile and CloseFile data blocks.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
           using globals
           using ParmBlocks

           sta   pOpen+$0a                ;resourceNumber
           _OpenGS pOpen
           bcs   Fail

           lda   pOpen+$02
           sta   pWrite+$02
           sta   pClose+$02

Done       ANOP
           clc
           rts

Fail       ANOP
           jsr   ToolError
           sec
           rts
           END

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
WriteFile START
;
; Calling conventions:
;   JSR with the #of bytes to read in Acc
;   Returns with carry set on failure
;
; All relevant details should already be initialized in pWrite.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
           using globals
           using ParmBlocks

           sta   pWrite+$08               ;requestCount
           stz   pWrite+$0a
           _WriteGS pWrite
           bcs   Fail
           lda   pWrite+$08               ;compare with transferCount
           cmp   pWrite+$0c
           beq   Done
           lda   #$004c                   ;hit eof early, I guess (huh?)
           bra   Fail                     ; ($004c is eofEncountered)

Done       ANOP
           clc
           rts

Fail       ANOP
           jsr   ToolError
           sec
           rts
           END

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
CloseFile  START
;
; Calling conventions:
;   JSR
;
; Closes the open output file, if any.  It's okay to call this if nothing
; is open.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
           using ParmBlocks

           lda   pClose+$02
           bmi   Done                     ;nothing to close

           _CloseGS pClose
           bcc   Done
           jsr   ToolError                ;print nasty message

Done       ANOP
           lda   #$ffff                   ;indicate nothing to close
           sta   pClose+$02
           clc
           rts
           END


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
SetFileInfo START
;
; Calling conventions:
;   JSR after setting up the pSetFileInfo block
;   Returns with carry set on error.
;
; Issues the _SetFileInfoGS call.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
           using ParmBlocks

           _SetFileInfoGS pSetFileInfo
           bcc   Done
           jsr   ToolError
           sec

Done       ANOP
           rts
           END


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
Extract    START
;
; Calling conventions:
;   JSR with the archive and output file positioned correctly
;   <ptr should be positioned to the thread header
;   Returns with carry set on error
;
; Extracts the data from a data_thread, placing it into the output file.
; Reads from and writes to the current position in each file.
;
; DP USAGE:
;   subtmp, subtmp2
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
           using globals
           using ParmBlocks

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

; Note we use the comp_thread_eof, not the thread_eof; the comp_thread_eof
; is ALWAYS the total length of the data stored in this thread.  By using
; c_t_e instead of t_e, we can use this routine to extract ANY thread,
; not just uncompressed data threads.  Of course, we'd have to flag the
; CRC, but that's trivial.
           lda   #$ffff
           sta   <crc
           lda   #$8001                   ;init % msg countdown
           sta   <percount

           ldy   <ptr
           lda   |$000c,y                 ;comp_thread_eof
           sta   <thread_eof              ;use some LZW DP spaces so we can
           sta   <othread_eof             ; also use their percentage printer
           lda   |$000e,y
           sta   <thread_eof+2
           sta   <othread_eof+2

           lda   <thread_eof              ;is there anything to do here?
           ora   <thread_eof+2
           bne   notzero                  ;yes, do it
           jmp   Done                     ;no, exit
notzero    anop

           lda   #buffer
           sta   pArcRead+$04             ;dataBuffer
           sta   pWrite+$04               ;dataBuffer
           lda   #buffer|-16
           sta   pArcRead+$06
           sta   pWrite+$06

loop       anop
           lda   <thread_eof+2
           bne   huge
           lda   <thread_eof
           cmp   #dPakBufSiz+1
           bge   huge

; request is < dPakBufSiz, so do it all
           tay
           stz   <thread_eof              ;zero request
           bra   do_copy

huge       anop
           ldy   #dPakBufSiz              ;request count
           lda   <thread_eof              ;update thread_eof
           sec
           sbc   #dPakBufSiz
           sta   <thread_eof
           lda   <thread_eof+2
           sbc   #$0000
           sta   <thread_eof+2

do_copy    anop
           sty   pArcRead+$08             ;requestCount
           stz   pArcRead+$0a
           sty   pWrite+$08               ;requestCount

           _ReadGS pArcRead
           bcc   read_ok
           jmp   Fail
read_ok    anop
           lda   pArcRead+$08
           cmp   pArcRead+$0c             ;transferCount
           beq   do_write
           lda   #$004c                   ;hit eof early, I guess
           jmp   Fail                     ; ($004c is eofEncountered)

do_write   anop
           lda   <mode
           and   #dIntegrity
           bne   copy_ok                  ;just pretend to write it
           lda   pWrite+$08               ;count; sort of backward
           jsr   WriteFile
           bcs   Fail

copy_ok    anop
           lda   rhbuf+$08                ;version_number
           cmp   #$0002
           blt   nocrc

           ldy   pArcRead+$08             ;requestCount
           shortm
           phb
           lda   #buffer|-16
           pha
           plb                            ;switch to buffer's data bank
           longm
           lda   #buffer
           jsr   CalcCRC
           plb

nocrc      anop
           lda   <mode
           and   #dVerbose
           beq   noperc
           jsr   PrintPerc                ;print % done
noperc     anop
           lda   #$0001
           sta   <percount
           lda   <thread_eof
           ora   <thread_eof+2
           beq   Done
           jmp   loop

Done       ANOP
           lda   rhbuf+$08                ;version_number
           cmp   #$0002
           blt   fnocrc

           ldy   <ptr
           lda   |$0006,y
           cmp   <crc
           beq   fnocrc

           lda   #eBadThread              ;CRC match failed
           bra   Fail

fnocrc     anop
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
           clc
           rts

Fail       ANOP
           jsr   ToolError
           sec
           rts

extmsg1    dc    C'Extracting ',H'00'
extmsg2    dc    C'...',H'00'
rsrcmsg2   dc    C' (resource fork)...',H'00'
extdone    dc    C'done.',H'0d0a00'
backspc    dc    H'0808080800'
           END


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
ParmBlocks DATA
;
; This holds all of the GS/OS parameter blocks.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; generic GS/OS and shell stuff
;
pGetVersion ANOP
           dc    I2'1'                    ;pCount
           ds    2                        ;version

pSession   ANOP
           dc    I2'0'                    ;pCount

pInitWild  ANOP
           dc    A4'wfilename+1'
           dc    I2'0'                    ;allow prompting

pNextWild  ANOP
           dc    A4'archivename+1'        ;stuff it into the arc filename buf

;
; parm blocks for .CONSOLE
;
pConOpen   ANOP
           dc    I2'2'                    ;pCount
           dc    H'ffff'                  ;refNum
           dc    A4'console'              ;pathname

pConRead   ANOP
           dc    I2'4'                    ;pCount
           dc    H'ffff'                  ;refNum
           dc    A4'charbuf'              ;dataBuffer
           dc    I4'1'                    ;(one char at a time)
           ds    4                        ;transferCount
charbuf    ds    4

pConClose  ANOP
           dc    I2'1'                    ;pCount
           dc    H'ffff'

pGetDevNum ANOP
           dc    I'2'
           dc    A4'console'
           dc    H'ffff'
console    dc    I2'8',C'.CONSOLE'

pDStatus   ANOP
           dc    I'5'                     ;pCount
           dc    H'ffff'                  ;devNum
           dc    I2'$8005'                ;code (GetReadMode)
           dc    A4'statuslist'           ;list
           dc    I4'2'                    ;requestCount
           ds    4                        ;transferCount
statuslist ds    2

pDControl  ANOP
           dc    I'5'                     ;pCount
           dc    H'ffff'                  ;devNum
           dc    I2'$8003'                ;code (SetReadMode)
           dc    A4'controllist'          ;list
           dc    I4'2'                    ;requestCount
           ds    4                        ;transferCount
controllist dc   H'0080'                  ;set raw mode (hi bit set)

;
; parm blocks for manipulating the archive file
;
pArcOpen   ANOP
           dc    I2'3'                    ;pCount
           dc    H'ffff'                  ;refNum
           dc    A4'archivename'          ;pathname
           dc    I2'1'                    ;requestAccess (= rdonly)

pArcRead   ANOP
           dc    I2'5'                    ;pCount
           dc    H'ffff'                  ;refNum
           dc    A4'buffer'               ;dataBuffer
           ds    4                        ;requestcount
           ds    4                        ;transferCount
           dc    I2'0'                    ;cachePriority

pArcSetMark ANOP
           dc    I2'3'                    ;pCount
           dc    H'ffff'                  ;refNum
           ds    2                        ;base
           ds    4                        ;displacement

pArcClose  ANOP
           dc    I2'1'                    ;pCount
           dc    H'ffff'                  ;refNum

;
; blocks for the output file
;
pCreate    ANOP
           dc    I2'5'                    ;pCount (don't include EOFs)
           dc    A4'filename_buf'         ;pathname
           dc    I2'$00c3'                ;access
           dc    I2'0'                    ;fileType == NON
           dc    I4'0'                    ;auxType == 0
           dc    I2'0'                    ;storageType
           ds    8                        ;eof/resourceEof
pDestroy   ANOP
           dc    I2'1'                    ;pCount
           dc    A4'filename_buf'         ;pathname
pOpen      ANOP
           dc    I2'4'                    ;pCount
           dc    H'ffff'                  ;refNum
           dc    A4'filename_buf'         ;pathname
           dc    I2'2'                    ;requestAccess (= wronly)
           dc    I2'0'                    ;resourceNumber
pWrite     ANOP
           dc    I2'5'                    ;pCount
           dc    H'ffff'                  ;refNum
           dc    A4'buffer'               ;dataBuffer
           ds    4                        ;requestCount
           ds    4                        ;transferCount
           dc    I2'0'                    ;cachePriority
pClose     ANOP
           dc    I2'1'
           dc    H'ffff'                  ;indicates nothing is open
pSetFileInfo ANOP
           dc    I2'7'                    ;pCount (we ignore option lists)
           dc    A4'filename_buf'         ;pathname
           ds    2                        ;access
           ds    2                        ;fileType
           ds    4                        ;auxType
           ds    2                        ;reserved
           ds    8                        ;createDateTime
           ds    8                        ;modDateTime
           ds    2
pExpandPath ANOP
           dc    I2'3'                    ;pCount
           dc    A4'filename_buf'         ;inputPath
           dc    A4'expfilename'          ;outputPath
           dc    I2'0'

pCreateDir ANOP
           dc    I2'5'                    ;pCount
           dc    A4'wfilename'            ;pathname
           dc    I2'$c3'                  ;access
           dc    I2'$0f'                  ;fileType (DIR)
           dc    I4'0'                    ;auxType
           dc    I2'$000d'                ;storageType (subdir)

;; some debugging stuff
;pGetPrefix ANOP
;           dc    I2'2'
;           dc    I2'0'
;           dc    A4'oldprefix'
;oldprefix  dc    I2'256'
;           ds    256
;pDebugPrefix ANOP
;           dc    I2'2'
;           dc    I2'0'
;           dc    A4'Ram5'
;Ram5       dc    I2'6',C'/Ram5/'
;pResetPrefix ANOP
;           dc    I2'2'
;           dc    I2'0'
;           dc    A4'oldprefix+2'
;pQuit      ANOP
;           dc    I2'0'                    ;pCount - do nothing at all
           END

;;;
;Quit       START
;           using ParmBlocks
;
;           _QuitGS pQuit
;           brk   00
;           nop
;           nop
;           nop
;           END
