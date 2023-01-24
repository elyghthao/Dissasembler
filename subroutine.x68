    ; dissember current instruction in buffer than return 
    ; a0 start address
    ; a1 end address
    ; return 
    ; a0 address of next instruction 
    ; a1 address of disassember code 
    ; if start address >= end address return will be 0 on both a0 and a1
disassemblerRegs Reg d0-d6/a2-a3  
disassembler
    movem.l disassemblerRegs,-(sp)
    
    move.l a0,a2 ; save a0 in a2
    move.l a1,a3 ; save a1 in a3
    ; we need to bytes 
    ; so we check if we have 2 bytes
    move.l a3,d1
    move.l a2,d0
    sub.l d0,d1
    cmp.l #2,d1
    blt disassemblerReturn0 ; we need 2 bytes 
    
    
    clr.l d6 ; default value is 0 for update if the current instruction have more than 2 byte size
    ; write start address to instruction_str
    move.l a0,d0
    lea instruction_str,a1
    move.l #1,d1 ; d1 = 1 for padding zeros
    move.l #8,d2 ; for write  8 digit at max
    jsr Hex2Str
    
    ; append separate between address and instruction 
    lea instruction_str,a0
    lea separate ,a1 
    jsr strcat 
    
    move.w (a2),d5 
    ; d5 = instruction
    move.l d5,d0
    move.l #12,d1
    lsr.l d1,d0 ; d0 = opcode 
    beq disassemblerG1
    cmp.b #$d,d0
    beq disassemblerAdd
    cmp.b #5,d0
    beq disassemblerAddQ
    cmp.b #$c,d0
    beq disassemblerAnd
    cmp.b #$e,d0
    beq disassemblerAslAsrLslLsr ; ASL ASR LSL LSR ROL ROR
    cmp.b #6,d0
    beq disassemblerBcc ; bra included here too
    cmp.b #4,d0
    beq disassembler4 ; we check for jsr and lea and movem and nop and not and rts 
    blt disassemblerless4 ; we check for move and movea
    cmp.b #7,d0
    beq disassemblerMoveq
    cmp.b #8,d0
    beq disassemblerOr
    cmp.b #9,d0
    beq disassemblerSub
    jmp disassemblerError ; others we will decode them as data
disassemblerAdd
    ; append 'ADD'
    lea instruction_str,a0
    lea opcode_ADD,a1 
    jsr strcat
    jmp disassemblerAddSub
disassemblerAddQ
    ; append 'ADDQ'
    lea instruction_str,a0
    lea opcode_ADDQ,a1 
    jsr strcat
    move.l d5,d0
    lsr.l #6,d0
    and.l #3,d0 ; d0 = size 
    cmpi.l #3,d0
    beq disassemblerError 
    ; d0 = bit 8 must be 0
    move.l d5,d1
    lsr.l #8,d1
    and.l #1,d1
    bne disassemblerError 
    ; we write size 
    jsr OpcodeSize
    
    move.l d5,d0
    lsr.l #8,d0
    lsr.l #1,d0
    and.l #7,d0
    bne disassemblerAddQData
    move.l #8,d0
disassemblerAddQData
    ; append data
    jsr WriteData
    
    ; append ','
    lea instruction_str,a0
    lea comma,a1 
    jsr strcat
    
    move.l d5,d3
    lsr.l #3,d3
    andi.l #7,d3
    cmpi.b #1,d3
    bne disassemblerAddQEA
    ; here mode = 1
    move.l d5,d0
    lsr.l #6,d0
    and.l #3,d0 ; d0 = size
    beq disassemblerError ; error no byte with An
disassemblerAddQEA
    ; d3 = mode
    move.l d5,d0
    andi.l #$3f,d0 ; d0 = EFFECTIVE ADDRESS
    move.l #$7f,d1 ; here we skip An #data for destination 
    move.l d6,d2
    addi.l #2,d2
    move.l (a2,d2),d2
    ; we don't want size because #data not implemented here
    jsr effectiveAddress
    cmpi.l #0,d0
    beq disassemblerError
    add.l d1,d6 ; update d6 
    jmp disassemblerUpdate
disassemblerAnd
    ; append 'AND'
    lea instruction_str,a0
    lea opcode_AND,a1 
    jsr strcat
    jmp disassemblerOrAnd
disassemblerAslAsrLslLsr 
    move.l d5,d0
    lsr.l #6,d0
    andi.l #3,d0 ; d0 = size
    cmpi.b #3,d0
    beq disassemblerAslAsrLslLsrM
    ; here immediate or register 
    move.l d5,d1
    lsr.l #3,d1
    andi.l #3,d1 ; d1 = type (0 arithmetic) (1 logical) (3 rotate)
    cmpi.b #2,d1 ; bad type  
    beq disassemblerError 
    jmp disassemblerAslAsrLslLsrU
disassemblerAslAsrLslLsrM
    ; here memory
    move.l d5,d1
    lsr.l #8,d1
    lsr.l #1,d1
    andi.l #7,d1 ; d1 = type (0 arithmetic) (1 logical) (3 rotate)
    cmpi.b #2,d1 ; bad type  
    beq disassemblerError 
    cmpi.b #3,d1
    bgt disassemblerError 
    subi.l #2,d0 ; correct size
disassemblerAslAsrLslLsrU
    cmpi.b #0,d1
    beq disassemblerAslAsrLslLsrA
    cmpi.b #1,d1
    beq disassemblerAslAsrLslLsrL
    cmpi.b #3,d1
    beq disassemblerRolRor
    jmp disassemblerError 
disassemblerRolRor
    ; append 'RO'
    lea instruction_str,a0
    lea opcode_RO,a1 
    jsr strcat
    jmp disassemblerRolRorU
disassemblerAslAsrLslLsrA
    ; append 'A'
    jsr AppendA
    jmp disassemblerAslAsrLslLsrAL
disassemblerAslAsrLslLsrL
    ; append 'L'
    lea instruction_str,a0
    lea lChar,a1 
    jsr strcat
disassemblerAslAsrLslLsrAL
    ; append 'S'
    lea instruction_str,a0
    lea sChar,a1 
    jsr strcat
disassemblerRolRorU
    move.l d5,d2
    lsr.l #8,d2
    andi.l #1,d2 ; d2 = dr
    cmpi.b #0,d2
    beq disassemblerAslAsrLslLsrR
    ; here left
    ; append 'L'
    lea instruction_str,a0
    lea lChar,a1 
    jsr strcat
    jmp disassemblerAslAsrLslLsrUU
disassemblerAslAsrLslLsrR
    ; append 'R'
    lea instruction_str,a0
    lea rChar,a1 
    jsr strcat
disassemblerAslAsrLslLsrUU
    jsr OpcodeSize
    move.l d5,d0
    lsr.l #6,d0
    andi.l #3,d0 ; d0 = size
    cmpi.b #3,d0
    bne disassemblerAslAsrLslLsrIR
    ; here memory
    move.l d5,d0
    andi.l #$3f,d0 ; d0 = EFFECTIVE ADDRESS
    move.l #$7C,d1 ; here we skip Dn An #data for destination 
    move.l #2,d2
    move.l (a2,d2),d2
    ; we don't want size because #data not implemented here
    jsr effectiveAddress
    cmpi.l #0,d0
    beq disassemblerError
    add.l d1,d6 ; update d6 
    jmp disassemblerUpdate
disassemblerAslAsrLslLsrIR
    move.l d5,d0
    lsr.l #8,d0
    lsr.l #1,d0
    andi.l #7,d0 ; d0 = count register
    move.l d5,d1
    lsr.l #5,d1
    andi.l #1,d1 ; d1 = ir
    beq disassemblerAslAsrLslLsrI
    ; here register
    move.l #1,d1 ; here we do just Dn 
    ; we don't need to use d2
    ; we don't want size because #data not implemented here 
    jsr effectiveAddress
    cmpi.l #0,d0
    beq disassemblerError
    add.l d1,d6 ; update d6 
    jmp disassemblerAslAsrLslLsrIRU
disassemblerAslAsrLslLsrI
    ; append data
    jsr WriteData
disassemblerAslAsrLslLsrIRU
    ; append ','
    lea instruction_str,a0
    lea comma,a1 
    jsr strcat
    ; here register
    move.l d5,d0
    andi.l #7,d0 ; d0 = EFFECTIVE ADDRESS
    move.l #1,d1 ; here we do just Dn 
    ; we don't need to use d2
    ; we don't want size because #data not implemented here
    jsr effectiveAddress
    cmpi.l #0,d0
    beq disassemblerError
    add.l d1,d6 ; update d6 
    jmp disassemblerUpdate
disassemblerBcc ; bra included here too
    ; append 'B'
    lea instruction_str,a0
    lea bChar,a1 
    jsr strcat
    ; d0 = CONDITION 
    move.l d5,d0
    lsr.l #8,d0
    andi.l #$f,d0 ; d0 = CONDITION
    beq disassemblerBRA ; here BRA  case
    cmpi.l #$f,d0
    beq disassemblerBLE ; here BLE case
    cmpi.l #$e,d0
    beq disassemblerBGT ; here BGT case
    cmpi.l #$7,d0
    beq disassemblerBEQ ; here BEQ case
    ; we skip other cases  
    jmp disassemblerError 
disassemblerBRA
    ; append 'RA'
    lea instruction_str,a0
    lea opcode_RA,a1 
    jsr strcat
    jmp BriefExtensionWordFormatU
disassemblerBLE
    ; append 'LE'
    lea instruction_str,a0
    lea opcode_LE,a1 
    jsr strcat
    jmp BriefExtensionWordFormatU
disassemblerBGT
    ; append 'GT'
    lea instruction_str,a0
    lea opcode_GT,a1 
    jsr strcat
    jmp BriefExtensionWordFormatU
disassemblerBEQ
    ; append 'EQ'
    lea instruction_str,a0
    lea opcode_EQ,a1 
    jsr strcat
BriefExtensionWordFormatU
    move.l d5,d0
    andi.l #$ff,d0 ; d0 = displacement
    move.l a2,d1
    move.l #2,d2
    move.l (a2,d2),d2
    jsr BriefExtensionWordFormat
    add.l d1,d6 ; update d6
    jmp disassemblerUpdate
    
disassembler4  ; we check for jsr and lea and movem and nop and not and rts 
    cmp.w #$4e71,d5
    beq disassemblerNop
    cmp.w #$4e75,d5
    beq disassemblerRts
    move.l d5,d0
    lsr.l #8,d0
    andi.l #$f,d0 ; d0 = 4 bit (bit 8 bit 9 bit 10 bit 11)
    cmpi.b #6,d0
    beq disassemblerNot
    cmpi.b #$e,d0
    beq disassemblerJsr
    move.l d5,d0
    lsr.l #6,d0
    andi.l #7,d0 ; d0 = Opmode
    ; opmode = 011 or opmode = 010 this mean movem 
    cmp.b #2,d0
    beq disassemblerMovem
    cmp.b #3,d0
    beq disassemblerMovem
    ; opmode = 111 this mean lea 
    cmp.b #7,d0
    beq disassemblerLea
    jmp disassemblerError 
    
disassemblerMovem
    move.l d5,d0
    lsr.l #7,d0
    andi.l #7,d0
    cmpi.b #1,d0
    bne disassemblerError ; d0 = 3 bit must be (001) ( bit 9 bit 8 bit 7 )
    move.l d5,d0
    lsr.l #8,d0
    lsr.l #3,d0
    andi.l #1,d0
    beq disassemblerError ; d0 = bit11 and must be 1
    ; here good format for moveq
    ; append 'MOVEM'
    lea instruction_str,a0
    lea opcode_MOVEM,a1 
    jsr strcat
    move.l d5,d0
    lsr.l #6,d0
    andi.l #1,d0 
    addi.l #1,d0 ; d0 = size
    jsr OpcodeSize
    move.l d5,d4
    lsr.l #8,d4
    lsr.l #2,d4
    andi.l #1,d4 ; d4 = dr
    beq disassemblerMovemRM 
    ; here Memory to register.
    move.l d5,d0
    andi.l #$3f,d0
    move.l #$6c,d1
    move.l #4,d2
    move.l (a2,d2),d2
    ; we don't need to use size no #DATA here
    jsr effectiveAddress
    cmp.b #0,d0
    beq disassemblerError 
    add.l d1,d6 ; update d6 
    ; append ','
    lea instruction_str,a0
    lea comma,a1 
    jsr strcat
    move.l #2,d0 ; d0 = 2
    move.w (a2,d0),d0 ; d0 = mask
    jsr registerList
    addi.l #2,d6 ; update d6 
    jmp disassemblerUpdate
disassemblerMovemRM 
    move.l d5,d1
    lsr.l #3,d1
    andi.l #7,d1 ; d1 = mode
    move.l #2,d0 ; d0 = 2
    move.w (a2,d0),d0 ; d0 = mask
    cmpi.b #4,d1
    bne disassemblerMovemRMU
    ; we need to reverse here
    jsr reverseBit16
disassemblerMovemRMU
    jsr registerList
    addi.l #2,d6 ; update d6
    ; append ','
    lea instruction_str,a0
    lea comma,a1 
    jsr strcat
    move.l d5,d0
    andi.l #$3f,d0
    move.l #$74,d1
    move.l #4,d2
    move.l (a2,d2),d2
    ; we don't need to use size no #DATA here
    jsr effectiveAddress
    cmp.b #0,d0
    beq disassemblerError 
    add.l d1,d6 ; update d6 
    jmp disassemblerUpdate
disassemblerLea
    ; append 'LEA '
    lea instruction_str,a0
    lea opcode_LEA,a1 
    jsr strcat
    move.l d5,d0
    andi.l #$fff,d0
    move.l #$64,d1 ; source (An) (xxx).w (xxx).l
    move.l #2,d2 ; destination An
    move.l (a2,d2),d3
    move.l d3,d4
    jsr disassemblerG2Fun
    cmpi.b #0,d0
    beq disassemblerError 
    add.l d1,d6 ; update d6 
    jmp disassemblerUpdate
disassemblerNot
    ; append 'NOT'
    lea instruction_str,a0
    lea opcode_NOT,a1 
    jsr strcat
    move.l d5,d0
    lsr.l #6,d0
    andi.l #3,d0
    jsr OpcodeSize
    move.l d5,d0
    andi.l #$3f,d0 ; d0 = EFFECTIVE ADDRESS
    move.l #$7D,d1 ; here for skip An and #DATA
    move.l d6,d2
    addi.l #2,d2
    move.l (a2,d2),d2
    ; we don't want size because #data not implemented here 
    jsr effectiveAddress
    cmpi.l #0,d0
    beq disassemblerError
    add.l d1,d6 ; update d6 
    jmp disassemblerUpdate
disassemblerJsr
    ; append 'JSR '
    lea instruction_str,a0
    lea opcode_JSR,a1 
    jsr strcat
    move.l d5,d0
    andi.l #$3f,d0 ; d0 = EFFECTIVE ADDRESS
    move.l #$64,d1 ; here for (An)  (xxx).W (xxx).L  and skip other 
    move.l d6,d2
    addi.l #2,d2
    move.l (a2,d2),d2
    ; we don't want size because #data not implemented here 
    jsr effectiveAddress
    cmpi.l #0,d0
    beq disassemblerError
    add.l d1,d6 ; update d6 
    jmp disassemblerUpdate
disassemblerNop
    ; append 'NOP'
    lea instruction_str,a0
    lea opcode_NOP,a1 
    jsr strcat 
    jmp disassemblerUpdate
disassemblerRts
    ; append 'RTS'
    lea instruction_str,a0
    lea opcode_RTS,a1 
    jsr strcat
    jmp disassemblerUpdate
disassemblerless4 ; we check for move and movea
    ; d0 = size
    move.l d5,d0
    lsr.l #8,d0
    lsr.l #4,d0
    andi.l #3,d0
    beq disassemblerError ; bad size
    cmpi.b #2,d0
    beq disassemblerless4SizeU
    ; here d0 = 1 or d0 = 11
    cmpi.b #1,d0
    beq disassemblerless4SizeB
    ; here d0 = 11 word 
    subi.l #2,d0 ; d0 = 01
    jmp disassemblerless4SizeU
disassemblerless4SizeB
    ; here d0 = 1
    subi.l #1,d0 ; d0 = 00
disassemblerless4SizeU
     
    ; append 'MOVE'
    lea instruction_str,a0
    lea opcode_MOVE,a1 
    jsr strcat
    move.l d5,d1
    lsr.l #6,d1
    andi.l #7,d1 ; d1 = destination mode
    move.l d5,d2
    lsr.l #3,d2
    andi.l #7,d2 ; d2 = source mode
    cmpi.b #7,d1
    bne disassemblerless4_7
    cmpi.b #7,d2
    bne disassemblerless4_7
    ; both mode = 7 
    move.l #2,d3
    move.l (a2,d3),d3
    cmpi.b #2,d0
    bne disassemblerless477BW
    move.l #6,d4
    jmp disassemblerless477BWL
disassemblerless477BW
    move.l #4,d4
disassemblerless477BWL
    move.l (a2,d4),d4
    jmp disassemblerless4SizeDone
disassemblerless4_7
    ; here one mode is 7 or both mod not 7
    move.l #2,d3
    move.l (a2,d3),d3
    move.l #2,d4
    move.l (a2,d4),d4
disassemblerless4SizeDone
    move.l d0,d5 ; d5 = size
    cmp.b #1,d1
    bne disassemblerless4U
    jsr AppendA
disassemblerless4U
    jsr OpcodeSize
    move.w (a2),d0
    andi.l #$fff,d0
    move.l #$ff,d1
    move.l #$7f,d2
    jsr disassemblerG3Fun
    cmpi.l #0,d0
    beq disassemblerError 
    add.l d1,d6 ; update d6 
    jmp disassemblerUpdate
disassemblerMoveq
    ; d0 = bit8
    move.l d5,d0
    lsr.l #8,d0
    andi.l #1,d0 ; d0 = bit8
    bne disassemblerError ; bad bit 
    ; append 'MOVEQ '
    lea instruction_str,a0
    lea opcode_MOVEQ,a1 
    jsr strcat 
    ; source
    move.l #$3c,d0 ; mode = 111 and register 100
    move.l #$80,d1 ; for just #DATA
    move.l d5,d2
    andi.l #$ff,d2 ; d2 = DATA
    move.l #2,d3 ; d3 = size = 10
    jsr effectiveAddress
    cmpi.l #0,d0
    beq disassemblerError
    ; we don't need to update d6 
    ; append ','
    lea instruction_str,a0
    lea comma,a1 
    jsr strcat
    move.l d5,d0
    lsr.l #8,d0
    lsr.l #1,d0
    andi.l #7,d0 ; d0 = register (d0 = EFFECTIVE ADDRESS) mode here = 0 because Dn
    move.l #1,d1 ; we do just Dn
    ; we don't want to use d2 because (xxx).L (xxx).W #DATA not used here 
    ; we don't want size because #data not implemented here :) ( so we don't need to use d3 )
    jsr effectiveAddress
    cmpi.l #0,d0
    beq disassemblerError
    ; we don't need to update d6 
    jmp disassemblerUpdate
disassemblerOr
    ; append 'OR'
    lea instruction_str,a0
    lea opcode_OR,a1 
    jsr strcat
    jmp disassemblerOrAnd
disassemblerSub
    ; append 'SUB'
    lea instruction_str,a0
    lea opcode_SUB,a1 
    jsr strcat
    jmp disassemblerAddSub
disassemblerG1 ; group of instruction instructions ( ADDI ANDI ORI SUBI )
    move.l d5,d0
    lsr.l #8,d0
    beq disassemblerORI
    cmp.b #6,d0
    beq disassemblerADDI
    cmp.b #2,d0
    beq disassemblerANDI
    cmp.b #4,d0
    beq disassemblerSUBI
    jmp disassemblerError
disassemblerADDI
    ; append 'ADDI' 
    lea instruction_str,a0
    lea opcode_ADDI,a1 
    jsr strcat 
    jmp disassemblerG1U
disassemblerANDI
    ; append 'ANDI' 
    lea instruction_str,a0
    lea opcode_ANDI,a1 
    jsr strcat 
    jmp disassemblerG1U
disassemblerORI
    ; append 'ORI' 
    lea instruction_str,a0
    lea opcode_ORI,a1 
    jsr strcat 
    jmp disassemblerG1U
disassemblerSUBI
    ; append 'SUBI' 
    lea instruction_str,a0
    lea opcode_SUBI,a1 
    jsr strcat 
disassemblerG1U
    move.l d5,d0
    lsr.l #6,d0
    andi.l #3,d0
    jsr OpcodeSize
    
    ; source
    move.l #$3c,d0 ; mode = 111 and register 100
    move.l #-1,d1 
    move.l d6,d2
    addi.l #2,d2
    move.l (a2,d2),d2
    move.l d5,d3
    lsr.l #6,d3
    andi.l #3,d3 ; d3 = size
    jsr effectiveAddress
    cmpi.l #0,d0
    beq disassemblerError
    add.l d1,d6 ; update d6 
    
    ; append ','
    lea instruction_str,a0
    lea comma,a1 
    jsr strcat
    
    move.l d5,d0
    andi.l #$3f,d0 ; d0 = EFFECTIVE ADDRESS
    move.l #$7D,d1 ; here we skip An #data  for destination 
    move.l d6,d2
    addi.l #2,d2
    move.l (a2,d2),d2
    ; we don't want size because #data not implemented here 
    jsr effectiveAddress
    cmpi.l #0,d0
    beq disassemblerError
    add.l d1,d6 ; update d6 
    jmp disassemblerUpdate
disassemblerOrAnd
    move.l #$fd,d1 ; we skip An 
    move.l #$7d,d2 ; we skip An also #DATA because destination can't be data
    jmp disassemblerG4Call
disassemblerAddSub
    move.l #$ff,d1 
    move.l #$7f,d2 ; we skip #DATA because destination can't be data
    move.l d5,d0
    lsr.l #6,d0
    andi.l #7,d0 ; d0 = opmode 
    cmpi.b #3,d0
    beq disassemblerAddSubAn
    cmpi.b #7,d0
    bne disassemblerG4Call
disassemblerAddSubAn
    jsr AppendA
disassemblerG4Call
    move.l d5,d0
    andi.l #$fff,d0
    move.l #2,d3
    move.l (a2,d3),d3
    move.l #6,d4
    move.l (a2,d4),d4
    jsr disassemblerG4Fun
    cmpi.b #0,d0
    beq disassemblerError
    add.l d1,d6 ; update d6
    jmp disassemblerUpdate
disassemblerData
    ; write start address to instruction_str
    move.l a2,d0
    lea instruction_str,a1
    move.l #1,d1 ; d1 = 1 for padding zeros
    move.l #8,d2 ; for write  8 digit at max
    jsr Hex2Str
    
    ; append separate between address and instruction 
    lea instruction_str,a0
    lea separate ,a1 
    jsr strcat 
    
    ; append 'DATA' 
    lea instruction_str,a0
    lea data ,a1 
    jsr strcat 
    
    ; append ' ' 
    lea instruction_str,a0
    lea Space,a1 
    jsr strcat 
    
    ; append '$' 
    lea instruction_str,a0
    lea sDolar,a1 
    jsr strcat 
    
    clr.l d0 ; d0 = 0
    move.w (a2),d0
    lea data_str,a1
    move.l #1,d1 ; d1 = 1 for padding zeros
    move.l #4,d2 ; for write  4 digit at max
    jsr Hex2Str
    
    ; append data to instruction_str
    lea instruction_str,a0
    lea data_str,a1 
    jsr strcat  
    
disassemblerUpdate
    move.l a2,a0
    adda.l #2,a0
    adda.l d6,a0 ; a0 address of next instruction 
    lea instruction_str,a1 ; a1 address of disassember code
disassemblerDone
    movem.l (a7)+,disassemblerRegs 
    rts
disassemblerReturn0
    suba.l a0,a0 ; a0 = 0
    suba.l a1,a1 ; a1 = 0
    jmp disassemblerDone
disassemblerError
    clr.l d6 ; d6 = 0 so we skip instruction we don't need the instruction length 
    jmp disassemblerData