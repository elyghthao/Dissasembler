; OpCodes
opcode_ADD dc.b 'ADD',0
opcode_ADDI dc.b 'ADDI',0
opcode_ADDQ dc.b 'ADDQ',0
opcode_SUB dc.b 'SUB',0
opcode_SUBI dc.b 'SUBI',0
opcode_AND dc.b 'AND',0
opcode_ANDI dc.b 'ANDI',0
opcode_OR dc.b 'OR',0
opcode_ORI dc.b 'ORI',0
opcode_NOP dc.b 'NOP',0
opcode_RTS dc.b 'RTS',0
opcode_LEA dc.b 'LEA ',0
opcode_GT dc.b 'GT ',0
opcode_LE dc.b 'LE ',0
opcode_EQ dc.b 'EQ ',0
opcode_RA dc.b 'RA ',0
opcode_JSR dc.b 'JSR ',0
opcode_NOT dc.b 'NOT',0
opcode_MOVE dc.b 'MOVE',0
opcode_MOVEQ dc.b 'MOVEQ ',0
opcode_MOVEM dc.b 'MOVEM',0
opcode_RO dc.b 'RO',0
data dc.b 'DATA',0
bChar dc.b 'B',0
wChar dc.b 'W',0
lChar dc.b 'L',0
Space dc.b ' ',0
dChar dc.b 'D',0
aChar dc.b 'A',0
sChar dc.b 'S',0
rChar dc.b 'R',0
qChar dc.b 'Q',0
mChar dc.b 'M',0
sDolar dc.b '$',0
comma dc.b ',',0
dot dc.b '.',0
octothorpe dc.b '#',0
octothorpe1 dc.b '#$',0
plus dc.b '+',0
minus dc.b '-',0
slash dc.b '/',0
separate dc.b '     ',0
BracketOpen dc.b '(',0
BracketClose dc.b ')',0
BracketClose1 dc.b ')+',0
BracketOpen1 dc.b '(A',0
BracketOpen2 dc.b '-(A',0
; string for user
welcomeMsg dc.b 'Enter starting address < ending address (no 0s leading ex.9000): ',0
enter_start_msg dc.b 'Enter starting loction (only hex characters and no 0s leading ex.9000): ',0
enter_end_msg dc.b 'Enter ending loction (only hex characters and no 0s leading ex.9000): ',0
error_msg1 dc.b 'Start location must be less than end location: ',0
next10instruction_msg dc.b 'Press Enter for more (other keys to exit): ',0
restart_msg dc.b 13,10,'Press y to restart (other keys to exit): ',0
start_odd_msg dc.b 'Start location address is odd, it must be even please try again: ',0
end_odd_msg dc.b 'End location address is odd, it must be even please try again: ',0
new_line  dc.b 13,10,0