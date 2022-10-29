;
;   The address range (xC000 - xFDFF) of the video output is restricted by the
;   Memory Protection Register (MPR) so that it can only be accessed in Supervisor mode.
;
;   On the other hand, whenever a call to a TRAP instruction is made, the LC-3 changes to Supervisor
;   mode and then it 'forgets' to change back to User mode when returning control to the user's application.
;   
;   Note: to fix this problem, the return statement of the TRAP instructions implemented by the LC-3 OS
;   should be RTT instead of RET
;
;   This program exploits said bug in the OS to get access to the video output's address range.
;
;   VIDEO OUTPUT DETAILS
;   - Video output is memory-mapped from address location xC000 to xFDFF. 
;   - The video display is 128 by 124 pixels (15,872 pixels total) and the coordinate system starts 
;     from (0,0) at the top left corner of the display, therefore: addr = xC000 + row*x0080 + col
;
;   See https://acg.cis.upenn.edu/milom/cse240-Fall05/handouts/lc3manual.html for more information about
;   the video output memory
;
;     
;

        .ORIG x3000
        LEA R0,SUPERVISOR_MODE_WARNING
        PUTS                            ; after this call, the privilege bit of the 
                                        ; PSR (Process Status Register) remains set
        
        ;;;;;;;;;;;;;;;;;;;;;
        ;; WHITE RECTANGLE
        ;;;;;;;;;;;;;;;;;;;;;
        LD R3,WHITE 
        LD R0,WHITE_TOP_LEFT_VERTEX        
        LD R1,WHITE_WIDTH  
        LD R2,WHITE_HEIGHT
        JSR PAINT_RECTANGLE

        ;;;;;;;;;;;;;;;;;;;;;
        ;; RED RECTANGLE
        ;;;;;;;;;;;;;;;;;;;;;
        LD R3,RED 
        LD R0,RED_TOP_LEFT_VERTEX        
        LD R1,RED_WIDTH  
        LD R2,RED_HEIGHT
        JSR PAINT_RECTANGLE

        ;;;;;;;;;;;;;;;;;;;;;
        ;; GREEN RECTANGLE
        ;;;;;;;;;;;;;;;;;;;;;
        LD R3,GREEN 
        LD R0,GREEN_TOP_LEFT_VERTEX        
        LD R1,GREEN_WIDTH  
        LD R2,GREEN_HEIGHT
        JSR PAINT_RECTANGLE

        HALT


;;;;;;;;;;;;;;;;;;;;;
;;; Paint rectangle
;;; addr = initial addr + row*x0080 + col
;;; (R0,R1,R2,R3) -> ()
;;; R0 = top left vertix address
;;; R1 = width
;;; R2 = height
;;; R3 = colour
;;;;;;;;;;;;;;;;;;;;;
PAINT_RECTANGLE
        ST R0,SAVE_R0
        ST R1,SAVE_R1
        ST R2,SAVE_R2
        ST R7,SAVE_R7

        ;; paint top horizontal line
        JSR PAINT_HORIZ_LINE

        ;; paint bottom horizontal line
        ADD R1,R2,#0            ; relative row 
        AND R2,R2,#0            ; relative col = 0   
        JSR RELATIVE_PIXEL_ADDRESS
        ADD R0,R5,#0
        LD R1,SAVE_R1
        JSR PAINT_HORIZ_LINE

        ;; paint left vertical line
        LD R4,LINE_LENGTH        
        LD R0,SAVE_R0
        LD R2,SAVE_R2
        JSR PAINT_VERT_LINE

        ;; paint right vertical line
        AND R1,R1,#0            ; relative row = 0
        LD R2,SAVE_R1           ; relative col  
        JSR RELATIVE_PIXEL_ADDRESS
        ADD R0,R5,#0
        LD R2,SAVE_R2
        JSR PAINT_VERT_LINE

        LD R0,SAVE_R0
        LD R1,SAVE_R1
        LD R2,SAVE_R2
        LD R7,SAVE_R7
        RET




;;;;;;;;;;;;;;;;;;;;;
;;; Paint vertical line
;;; addr = initial addr + row*x0080 + col
;;; (R0,R2,R3) -> ()
;;; R0 = initial address
;;; R2 = line length
;;; R3 = colour
;;;;;;;;;;;;;;;;;;;;;
PAINT_VERT_LINE
        ST R0,PVL_SAVE_R0       
        ST R1,PVL_SAVE_R1
        ST R2,PVL_SAVE_R2        
        ST R4,PVL_SAVE_R4 
        ST R7,PVL_SAVE_R7

        LD R4,LINE_LENGTH
        AND R1,R1,#0
        ADD R1,R1,#1        
LOOP2   BRz END_LOOP2
        JSR PAINT_HORIZ_LINE        
        ADD R0,R0,R4
        ADD R2,R2,#-1 
        BR LOOP2
END_LOOP2
        LD R0,PVL_SAVE_R0                    
        LD R1,PVL_SAVE_R1
        LD R2,PVL_SAVE_R2                     
        LD R4,PVL_SAVE_R4
        LD R7,PVL_SAVE_R7
        RET
PVL_SAVE_R0     .BLKW 1
PVL_SAVE_R1     .BLKW 1
PVL_SAVE_R2     .BLKW 1
PVL_SAVE_R4     .BLKW 1
PVL_SAVE_R7     .BLKW 1

;;;;;;;;;;;;;;;;;;;;;
;;; Paint horizontal line
;;; addr = initial addr + row*x0080 + col
;;; (R0,R1,R3) -> ()
;;; R0 = initial address
;;; R1 = line length
;;; R3 = colour
;;;;;;;;;;;;;;;;;;;;;
PAINT_HORIZ_LINE
        ST R1,PHL_SAVE_R1        
        ST R2,PHL_SAVE_R2   
        ADD R1,R1,#0    ; set condition codes 
LOOP1   BRz END_LOOP1
        ADD R2,R0,R1        
        STR R3,R2,#0
        ADD R1,R1,#-1
        BR LOOP1
END_LOOP1
        LD R1,PHL_SAVE_R1                     
        LD R2,PHL_SAVE_R2
        RET
PHL_SAVE_R1     .BLKW 1
PHL_SAVE_R2     .BLKW 1


;;;;;;;;;;;;;;;;;;;;;
;;; addr = initial addr + row*x0080 + col
;;; (R0,R1,R2) -> R5
;;; R0 = initial address
;;; R1 = row
;;; R2 = col
;;;;;;;;;;;;;;;;;;;;;
RELATIVE_PIXEL_ADDRESS                   
        ST R0,RPA_SAVE_R0        
        ST R2,RPA_SAVE_R2        
        ST R7,RPA_SAVE_R7
        
        ADD R5,R0,#0
        LD R0,LINE_LENGTH        
        JSR MULTIPLICATION          ; (R0,R1) -> R2
        ADD R5,R5,R2
        LD R2,RPA_SAVE_R2
        ADD R5,R5,R2

        LD R0,RPA_SAVE_R0                       
        LD R7,RPA_SAVE_R7
        RET
RPA_SAVE_R0     .BLKW 1
RPA_SAVE_R2     .BLKW 1
RPA_SAVE_R7     .BLKW 1


LINE_LENGTH             .FILL x0080 ; 128 pixels
WHITE                   .FILL x7FFF
GREEN                    .FILL x03E0
RED                     .FILL x7C00
WHITE_TOP_LEFT_VERTEX   .FILL xd001 ; (32,1)
WHITE_WIDTH             .FILL #64 ; pixels
WHITE_HEIGHT            .FILL #62 ; pixels
RED_TOP_LEFT_VERTEX     .FILL xd90a ; (50,10)
RED_WIDTH               .FILL #45 ; pixels
RED_HEIGHT              .FILL #15 ; pixels
GREEN_TOP_LEFT_VERTEX    .FILL xc564 ; (10,100)
GREEN_WIDTH              .FILL #10 ; pixels
GREEN_HEIGHT             .FILL #100 ; pixels
SUPERVISOR_MODE_WARNING  .STRINGZ "changing to Supervisor mode\n"

SAVE_R0     .BLKW 1
SAVE_R1     .BLKW 1
SAVE_R2     .BLKW 1
SAVE_R4     .BLKW 1
SAVE_R7     .BLKW 1

        .END
