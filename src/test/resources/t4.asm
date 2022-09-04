;
;   comment
;          
    .ORIG x3000
    JSR LABEL1
LABEL2
    ADD R0,R0,#1    
LABEL3 HALT

LABEL5

; multiple labels associated to the same instruction
LABEL1 ADD R0,R1,R2

LABEL4
    .END
