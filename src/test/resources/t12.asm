
    .ORIG x3000
    ADD R0, R0,#1 ; comment
    HALT
    .EXTERNAL SYMBOL_ON_OTHER_MODULE
    .END
    ADD R0, R0,#1 ; comment
