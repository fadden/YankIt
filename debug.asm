;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; debug.asm - stuff needed only during debugging
;
; (None of this is really necessary now that I'm using GSBug...)
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

           MLOAD 2/ainclude/m16.GSBug

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
PrintNum   START
;
; Print a number in hex and signed decimal.  Preserves all registers.
;
; Calling conventions:
;   JSL with number in Acc
;   Assumes DP is set
;
; DP USAGE:
;   $fa - $ff    This routine (debugging area)
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
print_num  equ   $fe
cp         equ   $fc                      ;convstr pointer
fp         equ   $fa                      ;formstr pointer
           using globals

           php
           rep   #$30
           LONGA ON
           LONGI ON
           phb
           phk
           plb

           phx
           phy
           sta   print_num

           lda   #|convstr                ;strcpy(convstr, formstr)
           sta   cp
           lda   #|formstr
           sta   fp

loop       lda   (fp)
           sta   (cp)
           beq   lpdone
           lda   fp                       ;two DP 16-bit INCs cost 16 cycles
           inc   A                        ;lda+inc+inc+lda = 12
           inc   A
           sta   fp
           lda   cp
           inc   A
           inc   A
           sta   cp
           bra   loop

lpdone     ANOP
           lda   <print_num               ;Int2Hex(num, convstr+6, 4)
           pha
           pea   convstr|-16
           pea   convstr+6
           pea   $0004
           _Int2Hex
           bcs   tool_error

           lda   <print_num               ;Int2Dec(num, convstr+12, 6, TRUE)
           pha
           pea   convstr|-16
           pea   convstr+12
           pea   $0006
           pea   $ffff
           _Int2Dec
           bcs   tool_error

           pea   convstr|-16
           pea   convstr
           _WriteCString
           bcs   tool_error

           lda   print_num                ;epilog
           ply
           plx
           plb
           plp
           rtl

tool_error ANOP
           pha
           pea   errorstr|-16
           pea   errorstr
           _SysFailMgr
           brk   0

formstr    dc    C'val=0x     (      )',H'0d0a000000'
convstr    ds    30
errorstr   dc    H'13',C'Crashed in PrintNum : '

           END


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
PrintLong  START
;
; Print a long integer in hex and signed decimal.  Preserves all registers.
;
; Calling conventions:
;   JSL with num in Acc and X-reg (lo, hi)
;   Assumes DP is set
;
; DP USAGE:
;   $f8 - $ff    This routine (debugging area)
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
print_num_hi equ $fe
print_num_lo equ $fc
cp         equ   $fa                      ;convstr pointer
fp         equ   $f8                      ;formstr pointer
           using globals

           php
           rep   #$30
           LONGA ON
           LONGI ON
           phb
           phk
           plb

           phy
           sta   print_num_lo
           stx   print_num_hi

           lda   #|convstr_hi             ;strcpy(convstr_hi, formstr_hi)
           sta   cp
           lda   #|formstr_hi
           sta   fp

loop_hi    lda   (fp)
           sta   (cp)
           beq   do_lo
           lda   fp                       ;two DP 16-bit INCs cost 16 cycles
           inc   A                        ;lda+inc+inc+lda = 12
           inc   A
           sta   fp
           lda   cp
           inc   A
           inc   A
           sta   cp
           bra   loop_hi

do_lo      lda   #|convstr_lo             ;strcpy(convstr_lo, formstr_lo)
           sta   cp
           lda   #|formstr_lo
           sta   fp

loop_lo    lda   (fp)
           sta   (cp)
           beq   lpdone                   ;note this expects $0000!!
           lda   fp                       ;two DP 16-bit INCs cost 16 cycles
           inc   A                        ;lda+inc+inc+lda = 12
           inc   A
           sta   fp
           lda   cp
           inc   A
           inc   A
           sta   cp
           bra   loop_lo

lpdone     ANOP
           lda   <print_num_hi            ;Int2Hex(num, convstr_hi+6, 4)
           pha
           pea   convstr_hi|-16
           pea   convstr_hi+6
           pea   $0004
           _Int2Hex
           bcc   no_err
           brl   tool_error

no_err     ANOP
           lda   <print_num_hi            ;Int2Dec(num, convstr+12, 6, TRUE)
           pha
           pea   convstr_hi|-16
           pea   convstr_hi+12
           pea   $0006
           pea   $ffff
           _Int2Dec
           bcs   tool_error

           lda   <print_num_lo            ;Int2Hex(num, convstr+6, 4)
           pha
           pea   convstr_lo|-16
           pea   convstr_lo+6
           pea   $0004
           _Int2Hex
           bcs   tool_error

           lda   <print_num_lo            ;Int2Dec(num, convstr+12, 6, TRUE)
           pha
           pea   convstr_lo|-16
           pea   convstr_lo+12
           pea   $0006
           pea   $ffff
           _Int2Dec
           bcs   tool_error

           pea   convstr_hi|-16           ;WriteCString(convstr_hi)
           pea   convstr_hi
           _WriteCString
           bcs   tool_error

           pea   convstr_lo|-16           ;WriteCString(convstr_lo)
           pea   convstr_lo
           _WriteCString
           bcs   tool_error

           lda   print_num_lo             ;epilog
           ldx   print_num_hi
           ply
           plb
           plp
           rtl

tool_error ANOP
           pha
           pea   errorstr|-16
           pea   errorstr
           _SysFailMgr
           brk   0

formstr_hi dc    C'val=0x     (      )  ',H'000000'
formstr_lo dc    C'val=0x     (      )',H'0d0a000000'
convstr_hi ds    30
convstr_lo ds    30
errorstr   dc    H'13',C'Crashed in PrintLong: '
           END

