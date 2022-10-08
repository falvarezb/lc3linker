;
;   implemenation of bitwise OR
;   
;   De Morgan's rule: X OR Y = NOT(NOT(X) AND NOT(Y))
;        
        .ORIG x3000
        LD R1,OPERAND1
        LD R2,OPERAND2
        NOT R1,R1
        NOT R2,R2
        AND R1,R1,R2
        NOT R1,R1        
        STI R1,RESULT_DEST
        HALT


OPERAND1    .FILL x0001
OPERAND2    .FILL x0010
RESULT_DEST .FILL x300F
        .END