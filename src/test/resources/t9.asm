;
;   comment
;          
    .ORIG x3000
    JSR LABEL
    .STRINGZ   "a\n'\\\t\eb"    
    ADD R0,R0,#1
    HALT

; label and instruction are in different lines
LABEL ; comment
    ADD R0,R1,R2 ; comment


    .END
