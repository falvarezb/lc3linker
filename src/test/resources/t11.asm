;
;   comment
;          
    .ORIG x3000
    ADD R0,R0,#1
LABEL
    ADD R0,R0,#1
    ADD R0,R1,R2 

; backwards jump
    JSR LABEL
    HALT
    .END
