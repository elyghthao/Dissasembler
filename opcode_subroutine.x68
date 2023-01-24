    ; This subrountine appends 'A' to instruction_str
    ; argument 
    ; d0 = 3 bit 
    ; none
    ; return none
WriteDataReg Reg d0/a0-a1
WriteData
    movem.l WriteDataReg,-(sp)
	andi.l #7,d0
	bne WriteDataU
	addi.l #8,d0 ; update value of d0
WriteDataU
    ; append '#'
    lea instruction_str,a0
    lea octothorpe,a1 
    jsr strcat
    
    addi.l #$30,d0 ; convert to digit
    move.b d0,(digit)
    ; append immediate.
    lea instruction_str,a0
    lea digit,a1 
    jsr strcat
    movem.l (sp)+,WriteDataReg
    rts

    ; this subrountine appends 'A' to instruction_str
    ; argument 
    ; none
    ; return none
AppendA
    movem.l a0-a1,-(sp)
    ; append 'A'
    lea instruction_str,a0
    lea aChar,a1 
    jsr strcat
    movem.l (sp)+,a0-a1
    rts
    ; this subrountine appends 'B' or 'W' or 'L' to instruction_str
    ; argument 
    ; d0 ( 0 byte 1 word 2 long )
    ; return none
OpcodeSize
    movem.l a0-a1,-(sp)
    ; append '.'
    lea instruction_str,a0
    lea dot,a1 
    jsr strcat
    cmpi.b #0,d0
    beq OpcodeSizeB
    cmpi.b #1,d0
    beq OpcodeSizeW
    cmpi.b #2,d0
    beq OpcodeSizeL
    jmp OpcodeSizeDone
OpcodeSizeB
    ; append 'B'
    lea instruction_str,a0
    lea bChar,a1 
    jsr strcat
    jmp OpcodeSizeDone
OpcodeSizeW
    ; append 'W'
    lea instruction_str,a0
    lea wChar,a1 
    jsr strcat
    jmp OpcodeSizeDone
OpcodeSizeL
    ; append 'L'
    lea instruction_str,a0
    lea lChar,a1 
    jsr strcat
OpcodeSizeDone
    ; append ' '
    lea instruction_str,a0
    lea Space,a1 
    jsr strcat
    movem.l (sp)+,a0-a1
    rts