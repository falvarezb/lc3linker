;
;   R0 -> R1   
;
;   subroutine to convert ASCII code of a char representing a number into
;   the binary representation of that number
;
ASCII_TO_BINARY
        LD R1,ASCII
        NOT R1,R1
        ADD R1,R1,#1
        ADD R1,R0,R1    ; convert ASCII to binary: R1 <- R0 - x0030
        RTT


ASCII	.FILL	x0030 ; ASCII code of '0'
        .END
