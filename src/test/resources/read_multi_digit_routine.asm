;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; R1 -> R2
;
; Reads the digits of a positive number from stdin, one digit at a time, and the result is returned in R2
; The number of digits to read is given by R1
;
; Algorithm: every new digit is added to the result of multiplying by 10 the previously calculated integer
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


READ_MULTI_DIGIT

    ; Save value of registers used as local variable
    ST R1, RMD_SAVE_R1
    ST R3, RMD_SAVE_R3
    ST R4, RMD_SAVE_R4
    ST R7, RMD_SAVE_R7 ; JSR and TRAP operations overwrite R7

    ;; variables initialisation                  
    AND R4, R4, #0  ; R4 = 0, R4 stores running computation   
    ADD R3, R1, #0  ; R3 is the loop counter   
RMD_LOOP
    BRz RMD_END_LOOP
    AND R0, R0, #0
    ADD R0, R0, #10 ; R0 = 10
    ADD R1, R4, #0
    JSR MULTIPLICATION  ; (R0,R1) -> R2
    ADD R4, R2, #0
    IN
    JSR ASCII_TO_BINARY ; R0 -> R1
    ADD R4, R4, R1
    ADD R3, R3, #-1 ; decrease loop counter
    BR RMD_LOOP

RMD_END_LOOP
    ADD R2, R4, #0

    ; Restore value of registers and return
    LD R1, RMD_SAVE_R1
    LD R3, RMD_SAVE_R3
    LD R4, RMD_SAVE_R4
    LD R7, RMD_SAVE_R7
    RET



    ; Memory segment to save registers used by this subroutine 
RMD_SAVE_R1 .BLKW #1
RMD_SAVE_R3 .BLKW #1
RMD_SAVE_R4 .BLKW #1
RMD_SAVE_R7 .BLKW #1

        .END