;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; util.asm - string manipulations, etc
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

           MLOAD YankIt.macros
           MLOAD 2/ainclude/m16.GSBug

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
Query      START
;
; Calling conventions:
;   JSR with the default character in the low byte of Acc.
;
; Returns the character recieved in low byte of Acc.  Accepts Y, N, and Q;
; returns default if RETURN is hit, Q if ESCAPE is hit.
;
; I could get fancy and use the nifty console driver stuff, but I'm not
; really in the mood right now.
;
; As with most user-interface code, this is a little convoluted.
;
; DP USAGE:
;  subtmp
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
           using globals
           using ParmBlocks

           and   #$00ff                   ;clean it up, just for kicks
           sta   <subtmp
           shortm
           sta   showdef+10
           longm

           pea   showdef|-16
           pea   showdef
           _ErrWriteCString

repeat     anop
           longm
           jsr   ReadConsole
           shortm                         ;shouldn't affect carry
           bcs   iscr                     ;use defaults (sorta dangerous)
           lda   charbuf
           and   #$7f
           cmp   #$60
           blt   isupper
           sec
           sbc   #$20
isupper    anop
           cmp   #"Y"
           beq   isok
           cmp   #"N"
           beq   isok
           cmp   #"Q"
           beq   isok
           cmp   #$0d                     ;return
           beq   iscr
           cmp   #$0a                     ;linefeed; also accepted
           beq   iscr
           cmp   #$1b                     ;escape
           beq   isesc
           bra   repeat

isesc      anop
           lda   #"Q"                     ;esc = Q
           bra   isok
iscr       anop
           lda   <subtmp

isok       anop
           cmp   #"Q"
           bne   notq
           inc   <abort                   ;abort it all
notq       anop
           sta   showres
           longm

           pha
           pea   showres|-16
           pea   showres
           _ErrWriteCString
           pla
           rts

showdef    dc    C' (y/n/q)? x',H'0800'
showres    dc    C'x',H'0d0a00'
           END


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
strcpy     START
;
; Calling conventions:
;   JSR with source bank offset in X and destination bank offset in Y, and
;     set Acc to be TRUE to copy the null, FALSE to exit w/o copying it.
;
; Works like C strcpy(); keeps going until it hits a '00'.
;
; DP USAGE:
;   subtmp
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
           using globals

           sta   <subtmp
           shortm
loop       ANOP
           lda   |$0000,x
           beq   Done
           sta   |$0000,y
           inx
           iny
           bra   loop

Done       ANOP
           ldx   <subtmp                  ;copy '\0' too?
           beq   Done2                    ;no, exit
           sta   |$0000,y                 ;yes, store it

Done2      anop
           longm
           rts
           END

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
strncpy    START
;
; Calling conventions:
;   JSR with source bank offset in X and destination bank offset in Y,
;     and maximum #of bytes to transfer in Acc.
;
; Works like C strncpy(); keeps going until it hits a '00' or the max #of
; bytes is reached.  The #of characters NOT transferred will be left in
; <subtmp.
;
; If the limit is reached before a NUL, the destination string will NOT
; be null-terminated.
;
; DP USAGE:
;   subtmp
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
           using globals

           sta   <subtmp
           shortm
loop       lda   |$0000,x
           sta   |$0000,y
           beq   Done
           inx
           iny
           longm
           dec   <subtmp                  ;note this is an 8-bit dec
           shortm
           bra   loop

Done       ANOP
           longm
           rts
           END

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
bcopy      START
;
; Calling conventions:
;   JSR with source address in ptr, destination address in ptr2, and the
;     #of bytes to transfer in the X-reg.
;
; Works like C bcopy().
;
; DP USAGE:
;   ptr, ptr2 (r/o)
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
           using globals

           shortm
           ldy   #$0000
loop       ANOP
           lda   [<ptr],y
           sta   [<ptr2],y
           iny
           dex
           bne   loop

Done       ANOP
           longm
           rts
           END


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
AsciiDate  START
;
; Calling conventions:
;   JSR after pushing arguments on as if issuing a _WriteTimeHex toolbox call
;
; There isn't a generic PrintDate routine in the toolbox, so we have to do
; it ourselves.  I set it up like a toolbox call on the off chance that I
; missed something in the manuals...
;
;       -- previous contents --
;          month    |  day
;          curYear  |  hour
;          minute   |  second
;          destination buffer (4 bytes)
;   sp-->
;
; DP USAGE:
;   subtmp, subtmp2
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
           using globals
;           SetStone #"PrintDate"          ;debug

           ply                            ;return addr
           pla
           sta   <subtmp
           pla
           sta   <subtmp2
           pla
           sta   >datebuf                 ;minute/second
           pla
           sta   >datebuf+2               ;hour/year
           pla
           sta   >datebuf+4               ;month/day
           phy                            ;put return addr back on

; first, verify that the date is reasonable
           shortm
           lda   >datebuf                 ;second
           cmp   #60
           bge   bad_date
           lda   >datebuf+1               ;minute
           cmp   #60
           bge   bad_date
           lda   >datebuf+2               ;hour
           cmp   #24
           bge   bad_date
           lda   >datebuf+4               ;day
           cmp   #31
           bge   bad_date
           lda   >datebuf+5               ;month
           cmp   #12
           bge   bad_date

; now see if there's anything there at all
           longm
           lda   >datebuf+0
           ora   >datebuf+2
           ora   >datebuf+4
           beq   no_date
           bra   got_date

no_date    ANOP
           ldy   #$000c
ndloop     lda   ndstr,y
           sta   [<subtmp],y
           dey
           dey
           bpl   ndloop
           brl   Done

bad_date   ANOP
           longm
           ldy   #$000c
bdloop     lda   bdstr,y
           sta   [<subtmp],y
           dey
           dey
           bpl   bdloop
           brl   Done

; the date is valid, so print it
got_date   ANOP
; okay, we got something, so stuff it into the string
           lda   #" -"                    ;stick some dashes in
           ldy   #$0002
           sta   [<subtmp],y
           ldy   #$0006
           sta   [<subtmp],y
           lda   #" :"                    ;add the colon
           ldy   #$000c
           sta   [<subtmp],y
; would be nice to stick leading zeroes in where appropriate, but the
; toolbox wants to put a space in front of decimal numbers...

           lda   >datebuf+4               ;day
           and   #$00ff
           pha
           lda   <subtmp+2
           pha
           lda   <subtmp
           pha
           pea   $0002                    ;2 chars
           pea   $0000
           _Int2Dec

           lda   >datebuf+3               ;year
           and   #$00ff
           pha
           lda   <subtmp+2
           pha
           lda   <subtmp
           clc
           adc   #$0007
           pha
           pea   $0002                    ;2 chars
           pea   $0000
           _Int2Dec

           lda   >datebuf+2               ;hour
           and   #$00ff
           pha
           lda   <subtmp+2
           pha
           lda   <subtmp
           clc
           adc   #$000a
           pha
           pea   $0002                    ;2 chars
           pea   $0000
           _Int2Dec

           lda   >datebuf+1               ;minute
           and   #$00ff
           pha
           lda   <subtmp+2
           pha
           lda   <subtmp
           clc
           adc   #$000d
           pha
           pea   $0002                    ;2 chars
           pea   $0000
           _Int2Dec

; do that month thang
           lda   >datebuf+5               ;month
           and   #$00ff
           asl   A
           asl   A
           tax
           lda   >months,x
           ldy   #$0003
           sta   [<subtmp],y
           lda   >months+1,x              ;some overlap here
           ldy   #$0004
           sta   [<subtmp],y

; add leading zeroes where needed
           shortm
z0         ldx   #"00"
           lda   [<subtmp]                ;day
           cmp   #$20
           bne   z1
           txa
           sta   [<subtmp]
z1         ldy   #$0007                   ;year
           lda   [<subtmp],y
           cmp   #$20
           bne   z2
           txa
           sta   [<subtmp],y
z2         ldy   #$000a                   ;hour
           lda   [<subtmp],y
           cmp   #$20
           bne   z3
           txa
           sta   [<subtmp],y
z3         ldy   #$000d                   ;minute
           lda   [<subtmp],y
           cmp   #$20
           bne   zdone
           txa
           sta   [<subtmp],y
zdone      longm

Done       ANOP
           clc
           rts

datebuf    ds    6
ndstr      dc    C'   [No Date]   '
bdstr      dc    C'   <invalid>   '
months     dc    C'Jan Feb Mar Apr May Jun Jul Aug Sep Oct Nov Dec '
           END

