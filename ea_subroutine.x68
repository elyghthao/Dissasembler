   * this subroutine appends size (b w l) and  effective address to instruction_str ( this is used for add sub or sub )
    ; argument 
    ; d0 = register1 (3 bit) opmode (3 bit) mode (3 bit) register2 (3 bit)
    ; d1 = mask1 for source ( this lets us know which mode that we can implement )
    ; d2 = mask2 for destination ( this lets us know which mode that we can implement )
    ; d3 = 4 bytes for (xxx).L or 2 byte for (xxx).W or #DATA for source
    ; d4 = 4 bytes for (xxx).L or 2 byte for (xxx).W or #DATA for destination
    ; return 
    ; d0 = 1 ( this means effective address added to instruction_str ) d0 = 0 ( this means effective address was not added to instruction_str )
    ; d1 = offset that we need to update ( for Dn An (An) (An)+ -(An) -(An) 2 for (xxx).W 4 for (xxx).L )
disassemblerG4FunRegs reg d2-d6/a0-a1
disassemblerG4Fun
    movem.l disassemblerG4FunRegs ,-(sp)
    ; d5 = opmode
    move.l d0,d6 ; save d0 in d6
    lsr.l #6,d0
    andi.l #7,d0
    cmpi.b #3,d0
    blt disassemblerG4FunSize
    beq disassemblerG4FunAnW
    cmpi.b #7,d0
    beq disassemblerG4FunAnL
    subi.l #4,d0
    jmp disassemblerG4FunSize
disassemblerG4FunAnw
    move.l #1,d0
    jmp disassemblerG4FunSize
disassemblerG4FunAnL
    move.l #2,d0
disassemblerG4FunSize
    jsr OpcodeSize
    move.l d0,d5
    move.l d6,d0
    jsr disassemblerG2Fun
disassemblerG4FunDone
    movem.l (sp)+,disassemblerG4FunRegs 
    rts
disassemblerG4FunReturn0
    clr.l d0
    clr.l d1
    jmp disassemblerG4FunDone

    * this subroutine appends the effective address to instruction_str ( this is used for add sub or sub lea )
    ; argument 
    ; d0 = register1 (3 bit) opmode (3 bit) mode (3 bit) register2 (3 bit)
    ; d1 = mask1 for source ( this lets us know which mode that can be implemented)
    ; d2 = mask2 for destination ( this lets us know which mode that can be implemented)
    ; d3 = 4 bytes for (xxx).L or 2 byte for (xxx).W or #DATA for source
    ; d4 = 4 bytes for (xxx).L or 2 byte for (xxx).W or #DATA for destination
    ; return 
    ; d0 = 1 ( this means effective address added to instruction_str ) d0 = 0 ( this mean effective address was not added to instruction_str )
    ; d1 = offset that we need to update ( for Dn An (An) (An)+ -(An) -(An) 2 for (xxx).W 4 for (xxx).L )
disassemblerG2FunRegs reg d2-d7/a0-a1
disassemblerG2Fun
    movem.l disassemblerG2FunRegs ,-(sp)
    move.l d0,d5
    lsr.l #6,d5
    andi.l #7,d5 ; d5 = opcode
    cmpi.b #3,d5
    blt disassemblerG2Fun_ea_Dn_Dn
    beq disassemblerG2AnW
    cmpi.b #7,d5
    blt disassemblerG2Fun_Dn_ea_Dn
    beq disassemblerG2AnL
disassemblerG2Fun_ea_Dn_Dn
    ; here opmode = 0 or 1 or 2
    ; update d0
    andi.l #$E3F,d0 ; clear bit (bit 8 bit 7 bit 6 ) Opmode = 000 ( 000 is the mode of Dn) 
    ; we don't need to use #DATA here because destination is Dn
    jsr disassemblerG3Fun
    jmp disassemblerG2FunDone
disassemblerG2Fun_Dn_ea_Dn
    ; here opmode = 4 or 5 or 6
    subi.l #4,d5 ; convert to size 
    ; swap d3 and d4
    move.l d3,d6
    move.l d4,d3
    move.l d6,d4
    ; update d0
    move.l d0,d6
    andi.l #7,d6
    lsl.l #8,d6
    lsl.l #1,d6
    move.l d0,d7
    lsr.l #8,d7
    lsr.l #1,d7
    andi.l #7,d7
    andi.l #$38,d0
    lsl.l #3,d0
    or.l d6,d0
    or.l d7,d0
    jsr disassemblerG3Fun
    jmp disassemblerG2FunDone
disassemblerG2AnW
    move.l #1,d5
    jmp disassemblerG2AnU
disassemblerG2AnL
    move.l #2,d5
disassemblerG2AnU
    ; update d0
    andi.l #$E3F,d0 ; clear bit (bit 8 bit 7 bit 6 )
    ori.l #$40,d0 ; set bit 6  here opmode = 001  this is mode of An 
    jsr disassemblerG3Fun
disassemblerG2FunDone
    movem.l (sp)+,disassemblerG2FunRegs 
    rts


    * this subroutine appends effective address to instruction_str (this is used for add sub or sub lea after convert formula and for move directly)
    ; argument 
    ; d0 = destination 6 bit{ Register 3 bit Mode 3 bit} source 6 bit{ Mode  3 bit register 3 bit}
    ; d1 = mask1 for source ( this lets us know which mode that can be implemented)
    ; d2 = mask2 for destination ( this lets us know which mode that can be implemented)
    ; d3 = 4 bytes for (xxx).L or 2 byte for (xxx).W or #DATA for source 
    ; d4 = 4 bytes for (xxx).L or 2 byte for (xxx).W or #DATA for destination 
    ; d5 = size ( 0 byte 1 word 2 long )
    ; return 
    ; d0 = 1 ( this means effective address added to instruction_str ) d0 = 0 ( this mean effective address was not added to instruction_str )
    ; d1 = offset that we need to update ( for Dn An (An) (An)+ -(An) -(An) 2 for (xxx).W 4 for (xxx).L )*
disassemblerG3FunArg3 equ 0
disassemblerG3FunArg4 equ 4
disassemblerG3FunArg5 equ 8
disassemblerG3FunRegs reg d2-d5/a0-a1
disassemblerG3Fun
    movem.l disassemblerG3FunRegs ,-(sp)
    move.l d0,d3
    lsr.l #3,d3
    andi.l #7,d3 ; d3 = mode source 
    move.l d0,d4
    lsr.l #6,d4
    andi.l #7,d4 ; d4 = mode destination
    cmp.b #1,d3
    beq disassemblerG3FunAn
    cmp.b #1,d4
    bne disassemblerG3FunAnDone
disassemblerG3FunAn
    cmp.b #0,d5
    beq disassemblerG3FunReturn0 ; when source or destination is An and size is byte this is an error 
    ; here mode source = 1 and mode destination = 1 and size ! 0 (byte)
disassemblerG3FunAnDone
    ; we do source first 
    move.l d0,d4 ; save d0 in d4
    andi.l #$3f,d0 ; keep source 
    move.l disassemblerG3FunArg4(sp),d2
    move.l d5,d3 ; d3 = size 
    jsr effectiveAddress
    cmpi.l #0,d0
    beq disassemblerG3FunReturn0
    move.l d1,d3 ; for return
    
    ; append ','
    lea instruction_str,a0
    lea comma,a1 
    jsr strcat
    
    ; destination must be not #DATA
    move.l d4,d5
    lsr.l #6,d5
    andi.l #7,d5 ; d5 mode of destination 
    move.l d4,d0
    lsr.l #8,d0
    lsr.l #1,d0
    andi.l #7,d0 ; d0 register of destination 
    cmpi.b #7,d5
    bne disassemblerG3FunDU
    cmpi.b #4,d0
    beq disassemblerG3FunReturn0 ; destination must be not #DATA
disassemblerG3FunDU
    ; we do destination 
    lsl.l #3,d5
    or.l d5,d0 ; d0 destination 
    move.l disassemblerG3FunArg3(sp),d1 ; d1 = mask2 of destination 
    move.l disassemblerG3FunArg5(sp),d2 ; d1 = 4 bytes for destination 
    ; we don't need to use d3 no #DATA here 
    jsr effectiveAddress
    cmpi.l #0,d0
    beq disassemblerG3FunReturn0
    add.l d3,d1
disassemblerG3FunReturn1
    move.l #1,d0
disassemblerG3FunDone
    movem.l (sp)+,disassemblerG3FunRegs 
    rts
disassemblerG3FunReturn0
    clr.l d0
    clr.l d1
    jmp disassemblerG3FunDone



    
    * this subroutine appends effective address to instruction_str
    ; argument 
    ; d0 = mode (3 bit) register (3 bit)
    ; d1 = mask ( this let us know which mode that can implement )
    ; d1 = ( bit 0  if set Dn implement if clear was not implemented ) 
    ;      ( bit 1  if set An implement if clear was not implemented )
    ;      ( bit 2  if set (An) implement if clear was not implemented )
    ;      ( bit 3  if set (An)+ implement if clear was not implemented )
    ;      ( bit 4  if set -(An) implement if clear was not implemented )
    ;      ( bit 5  if set (xxx).W implement if clear was not implemented )
    ;      ( bit 6  if set (xxx).L implement if clear was not implemented )
    ;      ( bit 7  if set #<data> implement if clear was not implemented )
    ; d2 = 4 bytes for (xxx).L or 2 byte for (xxx).W or #DATA
    ; d3 = size ( 0 byte 1 word 2 long )
    ; return 
    ; d0 = 1 ( this means effective address added to instruction_str ) d0 = 0 ( this means effective address was not added to instruction_str )
    ; d1 = offset that we need to update ( for Dn An (An) (An)+ -(An) -(An) 2 for (xxx).W 4 for (xxx).L )
effectiveAddressRegs reg d2-d5/a0-a1
effectiveAddress
    movem.l effectiveAddressRegs ,-(sp)
    move.l d3,d5 ; d5 = size 
    move.l d1,d3 ; d3 = mask
    move.l d0,d4
    andi.l #7,d4 ; d4 = register
    lsr.l #3,d0
    andi.l #7,d0 ; d0 = mode
    clr.l d1 ; the default value of d1 is zero 
    cmp.b #0,d0
    beq effectiveAddressDD  ; Register Direct Data
    cmp.b #1,d0
    beq effectiveAddressDA ; Register Direct Address
    cmp.b #2,d0
    beq effectiveAddressIA ; Register Indirect Address
    cmp.b #3,d0
    beq effectiveAddressIAPos  ; Register Indirect Address with Postincrement
    cmp.b #4,d0
    beq effectiveAddressIAPre ; Register Indirect Address with Predecrement
    cmp.b #7,d0
    beq effectiveAddress7  ; here Absolute Data Addressing Short or Absolute Data Addressing Long or Immediate
    jmp effectiveAddressReturn0
effectiveAddressDD ; Register Direct Data 
    andi.l #1,d3 ; bit 0
    beq effectiveAddressReturn0 
    ; append 'D'
    lea instruction_str,a0
    lea dChar,a1 
    jsr strcat
    jmp effectiveAddressDDA
effectiveAddressDA ; Register Direct Address
    andi.l #2,d3 ; bit 1
    beq effectiveAddressReturn0
    ; append 'A'
    lea instruction_str,a0
    lea aChar,a1 
    jsr strcat
effectiveAddressDDA ; here Data or Address register
    ; append register 
    addi.b #$30,d4 ; convert to digit
    move.b d4,(digit)
    lea instruction_str,a0
    lea digit,a1 
    jsr strcat
    jmp effectiveAddressReturn1
effectiveAddressIA ; Register Indirect Address
    andi.l #4,d3 ; bit 2
    beq effectiveAddressReturn0
    ; append '(A'
    lea instruction_str,a0
    lea BracketOpen1,a1 
    jsr strcat
    jmp effectiveAddressIAPreIA 
effectiveAddressIAPos ; Register Indirect Address with Postincrement
    andi.l #8,d3 ; bit 3
    beq effectiveAddressReturn0
    ; append '(A'
    lea instruction_str,a0
    lea BracketOpen1,a1 
    jsr strcat
    ; append register 
    addi.b #$30,d4 ; convert to digit
    move.b d4,(digit)
    lea instruction_str,a0
    lea digit,a1 
    jsr strcat
    ; append ')+'
    lea instruction_str,a0
    lea BracketClose1,a1 
    jsr strcat
    jmp effectiveAddressReturn1
effectiveAddressIAPre ; Register Indirect Address with Predecrement
    andi.l #$10,d3 ; bit 4
    beq effectiveAddressReturn0
    ; append '-(A'
    lea instruction_str,a0
    lea BracketOpen2,a1 
    jsr strcat
effectiveAddressIAPreIA ; here Register Indirect Address with Predecrement or Register Indirect Address
    ; append register 
    addi.b #$30,d4 ; convert to digit
    move.b d4,(digit)
    lea instruction_str,a0
    lea digit,a1 
    jsr strcat
    ; append ')'
    lea instruction_str,a0
    lea BracketClose,a1 
    jsr strcat
    jmp effectiveAddressReturn1
effectiveAddress7 ; here Absolute Data Addressing Short or Absolute Data Addressing Long or Immediate 
    cmpi.b #0,d4
    beq effectiveAddressADASL
    cmpi.b #1,d4
    beq effectiveAddressADASL
    cmpi.b #4,d4
    beq effectiveAddressI 
    jmp effectiveAddressReturn0
effectiveAddressADASL
    ; append '$'
    lea instruction_str,a0
    lea sDolar,a1 
    jsr strcat
    cmpi.b #1,d4
    beq effectiveAddressADAL
effectiveAddressADAS ; Absolute Data Addressing Short 
    andi.l #$20,d3 ; bit 5
    beq effectiveAddressReturn0
    lsr.l #8,d2 
    lsr.l #8,d2 ; we need just 2 byte
    addi.l #2,d1
    jmp effectiveAddressADASLDone
effectiveAddressADAL ; Absolute Data Addressing Long
    andi.l #$40,d3 ; bit 6
    beq effectiveAddressReturn0
    addi.l #4,d1
effectiveAddressADASLDone
    move.l d1,d3
    move.l d2,d0
    lea data_str,a1
    move.l d1,d2 
    lsl.l #1,d2 ; for write 4 or 8 digit at max
    move.l #1,d1 ; d1 = 1 for padding zeros
    jsr Hex2Str
    move.l d3,d1
    ; append data to instruction_str
    lea instruction_str,a0
    lea data_str,a1 
    jsr strcat
    jmp effectiveAddressReturn1
effectiveAddressI ; Immediate 
    andi.l #$80,d3 ; bit 7
    beq effectiveAddressReturn0
    ; append '#$'
    lea instruction_str,a0
    lea octothorpe1,a1 
    jsr strcat
    move.l #4,d3
    move.l #8,d4
    cmpi.b #0,d5
    beq effectiveAddressIB
    cmpi.b #2,d5
    beq effectiveAddressIU
    ; here word 
    move.l #4,d4
    jmp effectiveAddressIBU
effectiveAddressIB
    ; here byte
    move.l #2,d4
    move.l d2,d0
    lsr.l #8,d0
    lsr.l #8,d0
    andi.l #$ff00,d0
    bne effectiveAddressReturn0
effectiveAddressIBU
    lsr.l #8,d2 
    lsr.l #8,d2 ; we need just 2 byte for byte and word case
    subi.l #2,d3
effectiveAddressIU
    move.l d2,d0
    lea data_str,a1
    move.l #0,d1 ; d1 = 0 for non padding zeros
    move.l d4,d2 ; for write  8 or 4 or 2 digit at max
    jsr Hex2Str
    clr.l d1
    ; append data to instruction_str
    lea instruction_str,a0
    lea data_str,a1 
    jsr strcat
    add.l d3,d1
effectiveAddressReturn1    
    move.l #1,d0
effectiveAddressDone
    movem.l (sp)+,effectiveAddressRegs 
    rts
effectiveAddressReturn0
    clr.l d0
    clr.l d1
    jmp effectiveAddressDone




    * this subroutine append  DISPLACEMENT + address + 2 to instruction_str we use this for Bcc instruction
    ; argument 
    ; d0 = DISPLACEMENT 
    ; d1 = address 
    ; d2 = next 4 byte 
    ; return 
    ; d0 = 1 ( this means address added to instruction_str ) d0 = 0 ( this mean address was not added to instruction_str )
    ; d1 = offset that we need to update
BriefExtensionWordFormatRegs reg d2-d3/a0-a1
BriefExtensionWordFormat
    movem.l BriefExtensionWordFormatRegs,-(sp)
    clr.l d3 ; d3 = 0 we use it for offset 
    ; append '$'
    lea instruction_str,a0
    lea sDolar,a1 
    jsr strcat
    cmpi.l #0,d0
    beq BriefExtensionWordFormat16
    cmpi.l #$ff,d0
    beq BriefExtensionWordFormat32
    ; here we convert signed extend DISPLACEMENT 8 bit to 32 bit
    ext.w d0
    ext.l d0
    jmp BriefExtensionWordFormatUpdate
BriefExtensionWordFormat16
    addi.l #2,d3
    lsr.l #8,d2
    lsr.l #8,d2 ; we need just two bytes
    move.l d2,d0
    ext.l d0 ; convert 16 bit to 32 bit using signed extend
    jmp BriefExtensionWordFormatUpdate
BriefExtensionWordFormat32
    addi.l #4,d3
    move.l d2,d0
BriefExtensionWordFormatUpdate
    add.l d1,d0 ; add DISPLACEMENT to current address 
    addi.l #2,d0 ; plus two
    lea data_str,a1
    move.l #1,d1 ; d1 = 1 for padding zeros
    move.l #8,d2 ; for write  8 digit at max
    jsr Hex2Str
    ; append data to instruction_str
    lea instruction_str,a0
    lea data_str,a1 
    jsr strcat 
    move.l d3,d1
BriefExtensionWordFormatReturn1
    move.l #1,d0
BriefExtensionWordFormatDone
    movem.l (sp)+,BriefExtensionWordFormatRegs
    rts
BriefExtensionWordFormatReturn0
    clr.l d0
    clr.l d1
    jmp BriefExtensionWordFormatDone





    ; argument 
    ; d0 = register mask as (B1 B0)
    ; return
    ; d0 = register mask as (B0 B1)
reverseBit2
    movem.l d1-d2,-(sp)
    move.l d0,d1
    andi.l #1,d1
    lsl.l #1,d1
    move.l d0,d2
    andi.l #2,d2
    lsr.l #1,d2
    or.l d2,d1
    move.l d1,d0
    movem.l (sp)+,d1-d2
    rts
    ; argument 
    ; d0 = register mask as (B3 B2 B1 B0)
    ; return
    ; d0 = register mask as (B0 B1 B2 B3)
reverseBit4
    movem.l d1-d3,-(sp)
    move.l d0,d3 ; save d0 in d3
    andi.l #$3,d0
    jsr reverseBit2
    move.l d0,d1
    lsl.l #2,d1
    move.l d3,d0
    lsr.l #2,d0
    andi.l #$3,d0
    jsr reverseBit2
    or.l d1,d0
    movem.l (sp)+,d1-d3
    rts
    ; argument 
    ; d0 = register mask as (B7 B6 B5 B4 B3 B2 B1 B0)
    ; return
    ; d0 = register mask as (B0 B1 B2 B3 B4 B5 B6 B7)
reverseBit8
    movem.l d1-d3,-(sp)
    move.l d0,d3 ; save d0 in d3
    andi.l #$f,d0
    jsr reverseBit4
    move.l d0,d1
    lsl.l #4,d1
    move.l d3,d0
    lsr.l #4,d0
    andi.l #$f,d0
    jsr reverseBit4
    or.l d1,d0
    movem.l (sp)+,d1-d3
    rts
    
    ; argument 
    ; d0 = register mask as (D0 D1 D2 D3 D4 D5 D6 D7 A0 A1 A2 A3 A4 A5 A6 A7)
    ; return
    ; d0 = register mask as (A7 A6 A5 A4 A3 A2 A1 A0 D7 D6 D5 D4 D3 D2 D1 D0)
reverseBit16
    movem.l d1-d3,-(sp)
    move.l d0,d3 ; save d0 in d3
    andi.l #$ff,d0
    jsr reverseBit8
    move.l d0,d1
    lsl.l #8,d1
    move.l d3,d0
    lsr.l #8,d0
    andi.l #$ff,d0
    jsr reverseBit8
    or.l d1,d0
    movem.l (sp)+,d1-d3
    rts
    
    ; write register list to instruction_str (this function used for movem)
    ; d0 = register mask as (A7 A6 A5 A4 A3 A2 A1 A0 D7 D6 D5 D4 D3 D2 D1 D0) 
    ; return none 
registerListReg Reg d0-d4/a0-a1
registerList
    movem.l registerListReg,-(sp)
    ; we swap bit of data regitsers with bit of address registers
    move.l d0,d1 ; d1 = mask
    andi.l #$ff,d1 ; keep first byte
    lsl.w #8,d1
    lsr.w #8,d0
    or.w d1,d0
    clr.l d1 ; we use d1 as i counter
    ; we use d2 for get current 2 bits
    clr.l d3 ; we use d3 for put '/' (slashChar) or '-' (minusChar)  the default value of d3 = 0 (this mean no - minus)
    cmpi.l #0,d0
    beq registerListDone    
registerListLoop
    lsr.l #1,d0
    bcc registerListLoopUpdate
    ; here  C-flag is 1.
    ; d2 = current 2 bit
    move.l d0,d2
    andi.l #3,d2 ; d2 = current 2 bit
    cmpi.l #1,d3
    bne registerListLoopDA
    ; here d3 = 1
    cmpi.l #1,d2
    beq registerListLoopUpdate
    cmpi.l #3,d2
    beq registerListLoopUpdate
    ; d2 = 0 or d2 = 2 
registerListLoopDA
    cmpi.l #8,d1
    bge registerListLoopA 
    ; here Address register
    ; append 'A'
    lea aChar,a1 
    jsr strcat
    move.l d1,d4
    jmp registerListLoopU
registerListLoopA
    ; here Data register
    ; append 'D'
    lea dChar,a1 
    jsr strcat
    move.l d1,d4
    subi.l #8,d4
registerListLoopU
    cmpi.l #0,d0
    beq registerListLoopDone
    cmpi.l #1,d3
    beq registerListLoopMinusCheck
    ; here d3 = 0
    ; append register
    addi.l #$30,d4 ; convert to digit
    move.b d4,(digit)
    lea digit,a1 
    jsr strcat
    cmpi.l #0,d0
    beq registerListDone
    cmpi.b #0,d2
    beq registerListLoopSlash
    cmpi.l #2,d2
    beq registerListLoopSlash
    cmpi.l #1,d2
    beq registerListLoopMinus
    cmpi.l #3,d2
    beq registerListLoopMinus
registerListLoopMinusCheck
    cmpi.l #1,d2
    beq registerListLoopUpdate
    cmpi.l #3,d2
    beq registerListLoopUpdate
    ; d2 = 0 or d2 = 2
    addi.l #$30,d4 ; convert to digit
    move.b d4,(digit)
    lea instruction_str,a0
    lea digit,a1 
    jsr strcat
    clr.l d3 ; d3 = 0
    cmpi.l #0,d0
    beq registerListDone
    ; append '/'
	lea instruction_str,a0
    lea slash,a1 
    jsr strcat
    jmp registerListLoopUpdate
registerListLoopMinus
    cmpi.b #7,d1
    beq registerListLoopSlash
    ; i != 7 
    ; d2 = 1 d2 = 3
    ; append '-'
	lea instruction_str,a0
    lea minus,a1 
    jsr strcat
    move.l #1,d3 ; d3 = 1
    jmp registerListLoopUpdate
registerListLoopSlash
    ; d2 = 0 or d2 = 2
    ; append '/'
	lea instruction_str,a0
    lea slash,a1 
    jsr strcat
registerListLoopUpdate
    addi.l #1,d1 ; i++
    cmpi.l #0,d0
    bne registerListLoop
    jmp registerListDone
registerListLoopDone
    ; append register
	lea instruction_str,a0
    addi.l #$30,d4 ; convert to digit
    move.b d4,(digit)
    lea digit,a1 
    jsr strcat
registerListDone    
    movem.l (sp)+,registerListReg
    rts