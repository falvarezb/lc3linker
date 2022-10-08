;
;   implemenation of abs (absolute value) function
;          
        .ORIG x3000
        LD R1,OPERAND        
        BRn NEG        
        LEA R2,RESULT
        JMP R2
NEG     NOT R1,R1
        ADD R1,R1,#1
RESULT  STI R1,RESULT_DEST
        HALT


OPERAND         .FILL #-3
RESULT_DEST     .FILL x300F
        .END