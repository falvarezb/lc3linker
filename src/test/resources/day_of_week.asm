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

    ;;;;;;;;;;;;;;;;;;;;;;;
    ;; day of month
    ;;;;;;;;;;;;;;;;;;;;;;;
    LEA R0,DAY_MONTH_PROMPT
    PUTS 
    AND R1,R1,#0
    ADD R1,R1,#2
    JSR READ_MULTI_DIGIT    ; R1 -> R2
    ADD R6,R2,#0            ; f = k

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; month
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    LEA R0,MONTH_PROMPT
    PUTS 
    JSR READ_MULTI_DIGIT    ; R1 -> R2
    ST R2,MONTH
    ; calculate m
    ADD R0,R2,#-2        
    BRp MONTH_GT_2_1
    ADD R0,R0,#12           
MONTH_GT_2_1              
    ST R0,m
    
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; year
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    LEA R0,YEAR_PROMPT
    PUTS 
    AND R1,R1,#0
    ADD R1,R1,#4
    JSR READ_MULTI_DIGIT    ; R1 -> R2

    ; calculate year
    LD R0,MONTH
    ADD R0,R0,#-2        
    BRp MONTH_GT_2_2
    ADD R2,R2,#-1
MONTH_GT_2_2
    ST R2,YEAR
    ; calculate D
    ADD R1,R2,#0             
    LD R2,HUNDRED
    JSR DIVISION        ; (R1,R2) -> (R0,R1) ; R1 = D                            
    ST R1,D

    ; 13m
    LD R0,m
    AND R1,R1,#0
    ADD R1,R1,#13
    JSR MULTIPLICATION  ; (R0,R1) => R2                        
    
    ; 13m-1
    ADD R2,R2,#-1       ; 13m-1 = R2

    ; (13m-1)/5
    ADD R1,R2,#0        ; preparing R1 and R2 to call division
    AND R2,R2,#0
    ADD R2,R2,#5
    JSR DIVISION  
    ; k+(13m−1)/5
    ADD R6,R6,R0        
                        
    ; k+(13m−1)/5 + D
    LD R1,D          
    ADD R6,R6,R1        

    ; D/4
    AND R2,R2,#0
    ADD R2,R2,#4
    JSR DIVISION        
    ; k+(13m−1)/5 + D + D/4
    ADD R6,R6,R0      
                        
    ; calculate C
    LD R1,YEAR          
    LD R2,HUNDRED
    JSR DIVISION                               
    ST R0,C

    ; C/4
    ADD R1,R0,#0
    AND R2,R2,#0
    ADD R2,R2,#4
    JSR DIVISION                                
    ; k+(13m−1)/5 + D + D/4 + C/4
    ADD R6,R6,R0        
                        
    ; -2C
    LD R0,C
    ADD R0,R0,R0
    NOT R0,R0
    ADD R0,R0,#1
    ; k+(13m−1)/5 + D + D/4 + C/4 - 2C
    ADD R6,R6,R0        
    ST R6,f
                        
    ; f%7
    ADD R6,R6,#0    ; set condition codes
    BRzp POSITIVE_F
    NOT R6,R6
    ADD R6,R6,#1
POSITIVE_F
    ADD R1,R6,#0
    AND R2,R2,#0
    ADD R2,R2,#7
    JSR DIVISION        ; R1 contains remainder                        

    LD R6,f
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


MONTH               .BLKW 1
YEAR                .BLKW 1
HUNDRED             .FILL #100
f                   .BLKW 1
m                   .BLKW 1
C                   .BLKW 1
D                   .BLKW 1
R7_SAVE             .BLKW 1
DAY_MONTH_PROMPT    .STRINGZ         "Type day of the month\n"
MONTH_PROMPT        .STRINGZ         "Type month\n"
YEAR_PROMPT         .STRINGZ         "Type year\n"

DAYS    
    .STRINGZ    "SUNDAY   \n"
    .STRINGZ    "MONDAY   \n"
    .STRINGZ    "TUESDAY  \n"
    .STRINGZ    "WEDNESDAY\n"
    .STRINGZ    "THURSDAY \n"
    .STRINGZ    "FRIDAY   \n"
    .STRINGZ    "SATURDAY \n"

    .END
