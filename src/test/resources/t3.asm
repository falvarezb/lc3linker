;
;   comment
;          
    .ORIG x3000
    JSR LABEL
    ADD R0,R0,#1
    HALT

; label and instruction are in the same line
LABEL ADD R0,R1,R2


    .END
