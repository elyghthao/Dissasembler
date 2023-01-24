    * Appends a copy of the source string to the destination string. The terminating null character in destination is overwritten by the first character of source
    ; a0  = destination
    ; a1 = source
    ; return none
    
strcat
    move.l d0,-(sp)
    movem.l a0-a1,-(sp)
    ; first loop we need to get the address of first null char
strcatLoop1
    move.b (a0),d0
    beq strcatLoop2
    adda.l #1,a0 ; go to check next char
    jmp strcatLoop1
strcatLoop2
    move.b (a1)+,d0
    beq strcatDone
    move.b d0,(a0)+ 
    jmp strcatLoop2
strcatDone
    move.b d0,(a0) ; add null char at end
    movem.l (sp)+,a0-a1
    move.l (sp)+,d0
    rts
	
    ; convert value to hex 
    ; d0 value that we need to convert 
    ; a1 address of the buffer that will recieve string that converted 
    ; d1 = 0 no zero padding 1 with zero padding
    ; d2 = number of digit that we need to convert 
    ; return value none
Hex2Str
    movem.l d0-d4,-(sp)
    move.l a1,-(sp)
    clr.l d4 ; we use it as counter 
Hex2StrLoop
    move.l d0,d3
    lsr.l #4,d0 ; update d0
    andi.l #$f,d3
    cmp.b #10,d3
    blt Hex2StrLoopD
    ; here letter
    addi.l #$37,d3 ; 41-0a = 37
    jmp Hex2StrLoopS
Hex2StrLoopD ; here digit
    addi.l #$30,d3 ; convert to digit
Hex2StrLoopS ; we save it here 
    move.b d3,-(sp) ; push to stack
    addi #1,d4
    cmp.l #0,d0
    bne Hex2StrLoopU ; if there other no 0 digits we need to store them
    cmp.l #1,d1
    bne Hex2StrLoop2
Hex2StrLoopU ; update loop
    subi #1,d2
    bne Hex2StrLoop
Hex2StrLoop2
    ; pop from stack
    move.b (sp)+,d3
    move.b d3,(a1)+ ; save it
    subi #1,d4
    bne Hex2StrLoop2
    ; store null at end
    clr.b d3
    move.b d3,(a1)
    move.l (sp)+,a1
    movem.l (sp)+,d0-d4
    rts

    * calculate length of string 
    * a0 the string that we need to calculate it's length
    ; return 
    * d0 length of string

strlen
    move.l a0,-(sp)
    clr.l d0 ; d0 = 0
strlenLoop
    cmp.b #0,(a0)+
    beq strlenDone 
    addi.l #1,d0 ; d0++
    jmp strlenLoop
strlenDone
    move.l (sp)+,a0
    rts
