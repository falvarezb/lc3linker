;
;   calculate day of the week using Zeller's formula
;   f = k+(13m−1)/5+D+D/4+C/4−2C
;
;   k is the day of the month
;
;   if x is normal month number,
;   m=x+10 if x <= 2
;   m=x-2, otherwise
;
;   C is the first 2 digits of the year and D is the last 2 digits of the year, 
;   but if it is January or February, those of the previous year are used
;   
;          
    .ORIG x3000

    ;; Read day of month
    AND R1, R1, #0
    ADD R1, R1, #2
    JSR READ_MULTI_DIGIT
    ADD R6, R2, #0


    ;LD R6,DAY_OF_THE_MONTH    ; R6 is f

    ; calculate m and D

    ;; Read year
    AND R1, R1, #0
    ADD R1, R1, #4
    JSR READ_MULTI_DIGIT
    ADD R1, R2, #0
    ST R1,YEAR          
    LD R2,HUNDRED
    JSR DIVISION        ; R1 is overwritten with value of D (R1,R2) -> (R0,R1)
                        ; R2 can be discarded
    ST R1,D
    ;; Read month
    AND R1, R1, #0
    ADD R1, R1, #2
    JSR READ_MULTI_DIGIT
    ADD R0, R2, #0
    ;LD R0,MONTH     ; calculate month m and store in R0
    ADD R2,R0,#-2   ; R2 can be discarded
    BRp MONTH_GT_2
    ADD R0,R0,#12
    LD R1,D
    ADD R1,R1,#-1   ; using previous year   
    ST R1,D 
MONTH_GT_2
    LD R1,D
    ADD R0,R0,#-2   ; m = R0
    ST R1,D         ; R1 can be discarded

    ; calculate 13m
    AND R1,R1,#0
    ADD R1,R1,#13
    JSR MULTIPLICATION  ; R2 contains result of multiplication
                        ; R0 and R1 can be discarded
    
    ; calculate 13m-1
    ADD R2,R2,#-1       ; 13m-1 = R2

    ; calculate (13m-1)/5
    ADD R1,R2,#0        ; preparing R1 and R2 to call division
    AND R2,R2,#0
    ADD R2,R2,#5
    JSR DIVISION        ; R0 contains quotient
                        ; R1 and R2 can be discarded

    ADD R6,R6,R0        ; k+(13m−1)/5 = R6
                        ; R0 can be discarded
    ; calculate D
    LD R1,D          
    ADD R6,R6,R1        ; k+(13m−1)/5 + D = R6

    ; calculate D/4
    AND R2,R2,#0
    ADD R2,R2,#4
    JSR DIVISION        ; R1 is discarded
                        ; R0 contains quotient

    ADD R6,R6,R0        ; k+(13m−1)/5 + D + D/4 = R6
                        ; R0 can be discarded
    ; calculate C
    LD R1,YEAR          
    LD R2,HUNDRED
    JSR DIVISION        ; R0 is overwritten with value of C
                        ; R1 and R2 can be discarded
    ST R0,C

    ; calculate C/4
    ADD R1,R0,#0
    AND R2,R2,#0
    ADD R2,R2,#4
    JSR DIVISION        ; R0 is overwritten with value of quotient
                        ; R1 and R2 can be discarded

    ADD R6,R6,R0        ; k+(13m−1)/5 + D + D/4 + C/4 = R6
                        ; R0 can be discarded
    ; calculate -2C
    LD R0,C
    ADD R0,R0,R0
    NOT R0,R0
    ADD R0,R0,#1

    ADD R6,R6,R0        ; k+(13m−1)/5 + D + D/4 + C/4 - 2C = R6
                        ; R0 can be discarded
    ; f%7
    ADD R5,R6,#0
    BRzp POSITIVE_F
    NOT R5,R5
    ADD R5,R5,#1
POSITIVE_F
    ADD R1,R5,#0
    AND R2,R2,#0
    ADD R2,R2,#7
    JSR DIVISION        ; R1 contains remainder
                        ; R0,R2 can be discarded

    ADD R6,R6,#0
    BRzp POSITIVE_DAY_OF_WEEK
    NOT R1,R1
    ADD R1,R1,#1
    ADD R1,R1,#7

POSITIVE_DAY_OF_WEEK
    ; print day of week
    JSR PRINT_WEEK_DAY
    HALT


PRINT_WEEK_DAY
    ; the address of the corresponding day is DAYS + i*10
    LEA R0, DAYS
    ADD R1,R1,#0    ; to be able to use condition codes
LOOP    
    BRz OUTPUT
    ADD R0,R0,#11   ; go to next day
    ADD R1,R1,#-1   ; decrement loop variable
    BR LOOP
OUTPUT  
    ST R7,R7_SAVE       ; saving return address as PUTS will overwrite it
    PUTS
    LD R7,R7_SAVE
    RET


DAY_OF_THE_MONTH    .FILL #5
MONTH               .FILL #1
YEAR                .BLKW 1;.FILL #2022
HUNDRED             .FILL #100
C                   .BLKW 1
D                   .BLKW 1
R3_SAVE             .BLKW 1
R4_SAVE             .BLKW 1
R5_SAVE             .BLKW 1
R7_SAVE             .BLKW 1

DAYS    
    .STRINGZ    "SUNDAY   \n"
    .STRINGZ    "MONDAY   \n"
    .STRINGZ    "TUESDAY  \n"
    .STRINGZ    "WEDNESDAY\n"
    .STRINGZ    "THURSDAY \n"
    .STRINGZ    "FRIDAY   \n"
    .STRINGZ    "SATURDAY \n"

    .END
