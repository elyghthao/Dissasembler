    * return hex readed 
	; d0 = hex readed 
readHex
    movem.l d1-d2,-(sp)
    * we use d2 to save result
    clr.l d2 ; d2 = 0
readHexLoop
    ; read char from user 
    move #5,d0
    trap #15
    cmpi.l #$30,d1 * any char less '0' we end readHex
    blt readHexDone
    cmpi.l #$39,d1 * we check if digit
    ble readHexLoopD
    cmpi.l #$41,d1 * check with A
    blt readHexLoopCheck
    cmpi.l #$46,d1 ; check with F
    ble readHexLoopU
readHexLoopCheck
    cmpi.l #$61,d1 * check with a
    blt readHexDone
    cmpi.l #$66,d1 ; check with f
    bgt readHexDone
    * here letter lower case
    subi.l #$61,d1
    addi.l #10,d1 ; convert to value 
    jmp readHexLoopUpdate
readHexLoopU
    * here letter upper case
    subi.l #$41,d1
    addi.l #10,d1 ; convert to value 
    jmp readHexLoopUpdate
readHexLoopD
    subi.l #$30,d1
readHexLoopUpdate
    lsl.l #4,d2 ; update result!
    add.l d1,d2 * add current digit 
    jmp readHexLoop
readHexDone
    move.l d2,d0 
    movem.l (sp)+,d1-d2
    rts  