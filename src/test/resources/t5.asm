;
;   comment
;          
    .ORIG x3000
    JSR LABEL1
    ADD R0,R0,#1    
    HALT

; illegal: 2 labels in the same line
LABEL1 LABEL2 ADD R0,R1,R2

    .END
