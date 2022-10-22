;
;   integer division : calculate quotient and remainder
;   X/Y = q*y + r
;   IN: R1=X, R2=Y
;   OUT: R0=q, R1=r
;
;   Note: only valid for positive numbers
;    

DIVISION          
    ST R3, DIV_SAVE_R3      ; saving value of register to be used as local variable
    AND R0,R0,#0        ; R0 is quotient    

    NOT R2,R2
    ADD R2,R2,#1        ; R2=-Y

DIV_LOOP
    ADD R3,R1,R2
    BRn DIV_END_LOOP
    ADD R0,R0,#1
    ADD R1,R3,#0
    BR DIV_LOOP

DIV_END_LOOP
    LD R3, DIV_SAVE_R3      ; restoring value of register used as loca variable
    RET

DIV_SAVE_R3 .BLKW #1

    .END