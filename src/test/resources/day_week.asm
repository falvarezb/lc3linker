;
;   program that keeps prompting for an integer 'i' in the range 0-6,
;   and each time it outputs the corresponding name of the day.
;   If a key other than ’0’ through ’6’ is pressed, the program exits.
;

        .ORIG x3000
        LEA R0,PROMPT
        PUTSP
        GETC            ; R0 <- ASCII value of input char
        LEA R1,ASCII_TO_BINARY
        JSRR R1

        ; the address of the corresponding day is DAYS + i*10
        LEA R0, DAYS
        ADD R1,R1,#0    ; to be able to use condition codes
LOOP    BRz OUTPUT
        ADD R0,R0,#10   ; go to next day
        ADD R1,R1,#-1   ; decrement loop variable
        BR LOOP

OUTPUT  TRAP x22        ; PUTS
        HALT

PROMPT  .FILL x7954     ; Ty
        .FILL x6570     ; pe
        .FILL x0a3a     ; : 
        .FILL x0

DAYS    .STRINGZ    "Sunday   "
        .STRINGZ    "Monday   "
        .STRINGZ    "Tuesday  "
        .STRINGZ    "Wednesday"
        .STRINGZ    "Thursday "
        .STRINGZ    "Friday   "
        .STRINGZ    "Saturday "

        .END
