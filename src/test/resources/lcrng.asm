;
;   linear congruential random number generator
;   x(n) = a*x(n-1) mod m
;   m = 32767 (2**15-1)
;   a=7
;   seed x(0)=10
;
;   using Schrage's method to avoid overflow in intermediate results
;          
    .ORIG x3000

    LD R1,M
    LD R2,A
    JSR DIVISION
    ST R0,SAVE_Q
    ST R1,SAVE_R

    LD R1,SEED      ; R1 starts as x(0) and will take all successive values: x(1),x(2), ...
    LD R5,N         ; loop counter   

LOOP
    BRz END_LOOP
    LD R2,SAVE_Q
    JSR DIVISION    ; X/Q
    ST R0,SAVE_XDIVQ
    LD R0,A
    JSR MULTIPLICATION
    ADD R3,R2,#0    ; saving multiplication result in R3
    LD R0,SAVE_R
    LD R1,SAVE_XDIVQ
    JSR MULTIPLICATION
    NOT R2,R2
    ADD R2,R2,#1    ; negating multiplication result
    ADD R1,R2,R3
    BRn SUM
    BR NOT_SUM
SUM
    LD R2,M
    ADD R1,R1,R2
NOT_SUM
    JSR WRITE
    LD R0,NEWLINE
    OUT
    ADD R5,R5,#-1   ; updating loop counter
    BR LOOP

END_LOOP

    HALT

NEWLINE .FILL #10
A       .FILL #7
M       .FILL #32767
SEED    .FILL #10
N       .FILL #10   ; number of random numbers to generate
SAVE_Q      .BLKW #1
SAVE_R      .BLKW #1
SAVE_R1     .BLKW #1
SAVE_R2     .BLKW #1
SAVE_R3     .BLKW #1
SAVE_R4     .BLKW #1
SAVE_R5     .BLKW #1
SAVE_R6     .BLKW #1
SAVE_R7     .BLKW #1
SAVE_XDIVQ  .BLKW #1

MULTIPLICATION          ; R0=multiplicand, R1=multiplier, R2=result
    ST R3,SAVE_R3
    ST R4,SAVE_R4
    ST R5,SAVE_R5

    AND R2,R2,#0        ; initialise R2=0
    AND R3,R3,#0;       
    ADD R3,R3,#1        ; initialise R3=1
                        ; R3 is used to examine set bits in R1
    
    AND R4,R4,#0       
    ADD R4,R4,#15       ; initialise R4=15, loop counter
LOOP_MULT
    BRn END_LOOP_MULT
    AND R5,R1,R3
    BRz SHIFT_LEFT
    ADD R2,R2,R0
SHIFT_LEFT
    ADD R0,R0,R0        ; in binary, shift left equals multiplying by 2

    ADD R3,R3,R3        ; shifting bit 1 to the left
    ADD R4,R4,#-1       ; decrease counter
    BR LOOP_MULT

END_LOOP_MULT
    LD R3,SAVE_R3
    LD R4,SAVE_R4
    LD R5,SAVE_R5
    RET

DIVISION                ; R1=dividend and remainder, R2=divisor, R0=quotient
    AND R0,R0,#0    
    NOT R2,R2
    ADD R2,R2,#1        ; R2=-Y

LOOP_DIV
    ADD R3,R1,R2
    BRn END_LOOP_DIV
    ADD R0,R0,#1
    ADD R1,R3,#0
    BR LOOP_DIV

END_LOOP_DIV
    RET

WRITE               ; R1 is number to write
    ST R1,SAVE_R1
    ST R2,SAVE_R2
    ST R3,SAVE_R3
    ST R4,SAVE_R4
    ST R5,SAVE_R5
    ST R6,SAVE_R6
    ST R7,SAVE_R7

    LD R5,ASCII
    LD R6,STACK_BASE

    AND R2,R2,#0
    ADD R2,R2,#-10  ; R2=-10

LOOP_WRITE
    ADD R4,R1,R2
    BRn END_LOOP_WRITE
    JSR DIVISION2
    ADD R0,R1,R5
    JSR PUSH
    ADD R1,R3,#0    ; quotient is the new dividend
    BR LOOP_WRITE

END_LOOP_WRITE
    ADD R0,R1,R5
    JSR PUSH

    ; printing digits from the stack
STACK_LOOP
    JSR ISEMPTY
    BRp END_STACK_LOOP
    JSR POP
    OUT
    BR STACK_LOOP

END_STACK_LOOP
    LD R1,SAVE_R1
    LD R2,SAVE_R2
    LD R3,SAVE_R3
    LD R4,SAVE_R4
    LD R5,SAVE_R5
    LD R6,SAVE_R6
    LD R7,SAVE_R7
    RET

ASCII   .FILL x0030
STACK_BASE .FILL x4000
MINUS_STACK_BASE .FILL xc000 ; -x4000

DIVISION2                ; R1=dividend and remainder, R2=divisor, R3=quotient
    AND R3,R3,#0           

LOOP_DIV2
    ADD R4,R1,R2
    BRn END_LOOP_DIV2
    ADD R3,R3,#1
    ADD R1,R4,#0
    BR LOOP_DIV2

END_LOOP_DIV2
    RET

PUSH
    STR R0,R6,#0
    ADD R6,R6,#1
    RET

POP
    ADD R6,R6,#-1
    LDR R0,R6,#0
    RET

ISEMPTY
    AND R0,R0,#0
    LD R5,MINUS_STACK_BASE
    ADD R1,R6,R5
    BRz EMPTY_STACK 
    ADD R0,R0,#0   
    BR END_ISEMPTY
EMPTY_STACK
    ADD R0,R0,#1
END_ISEMPTY
    RET

    .END

