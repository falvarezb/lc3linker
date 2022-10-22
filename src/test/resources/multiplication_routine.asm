;
;   fast multiplication using shift-and-add algorithm
;   X*Y
;   IN: R0=X, R1=Y
;   OUT: R2=X*Y
;          

MULTIPLICATION
    ST R3,MULT_SAVE_R3
    ST R4,MULT_SAVE_R4
    ST R5,MULT_SAVE_R5

    AND R2,R2,#0        ; initialise R2=0
    AND R3,R3,#0;       
    ADD R3,R3,#1        ; initialise R3=1
                        ; R3 is used to examine set bits in R1
    
    AND R4,R4,#0       
    ADD R4,R4,#15       ; initialise R4=15, loop counter
MULT_LOOP
    BRn MULT_END_LOOP
    AND R5,R1,R3
    BRz MULT_SHIFT_LEFT
    ADD R2,R2,R0
MULT_SHIFT_LEFT
    ADD R0,R0,R0        ; in binary, shift left equals multiplying by 2

    ADD R3,R3,R3        ; shifting bit 1 to the left
    ADD R4,R4,#-1       ; decrease counter
    BR MULT_LOOP

MULT_END_LOOP
    LD R3,MULT_SAVE_R3
    LD R4,MULT_SAVE_R4
    LD R5,MULT_SAVE_R5
    RET

MULT_SAVE_R3     .BLKW 1
MULT_SAVE_R4     .BLKW 1
MULT_SAVE_R5     .BLKW 1

    .END