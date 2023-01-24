*-----------------------------------------------------------
* Title      : Main File
*-----------------------------------------------------------
    ORG $1000
MAIN:
    MOVEA.L #$00100000,SP
    move #13,d0
    lea welcomeMsg,a1
    trap #15
MAINagain
    move #14,d0
    lea enter_start_msg,a1
    trap #15
    
    jsr readHex
    move.l d0,d1
    andi.l #1,d1
    beq MAINstratSave
    ; odd address 
    ; print error message
    move #13,d0
    lea start_odd_msg,a1
    trap #15
    jmp MAINagain
MAINstratSave
    move.l d0,(start_location)
MAINendAgin
    move #14,d0
    lea enter_end_msg,a1
    trap #15
    
    jsr readHex
    move.l d0,d1
    andi.l #1,d1
    beq MAINendSave
    ; odd address 
    ; print error message
    move #13,d0
    lea end_odd_msg,a1
    trap #15
    jmp MAINendAgin
MAINendSave
    move.l d0,(end_location)
    
    cmp.l (start_location),d0
    ble MAINerror_1
    
    move.l (start_location),d2
    move.l d2,a0
MAINloop
    clr.l d1 ; we use d1 as counter of inner loop (i=0)
MAINInnerLoop
    cmpi.l #10,d1
    bge MAINInnerLoopDone
    ; here ( i < 10 )
    move.l (end_location),d0
    move.l d0,a1
    jsr disassembler
    cmp.l #0,a0
    beq MAINloop_done
    ; print instruction to user
    move #13,d0
    trap #15
    addi.l #1,d1 ; i++
    jmp MAINInnerLoop
MAINInnerLoopDone
    ; ask user if he wants to print next 10 instruction
    move #14,d0
    lea next10instruction_msg,a1
    trap #15
    ; wait for user to hit enter key
    move.l #5,d0
    trap #15
    cmpi.b #13,d1
    beq MAINClear
MAINloop_done
    ; ask user if he wants to restart or finish the program
    move #14,d0
    lea restart_msg,a1
    trap #15
    ; read option from user 
    move.l #5,d0
    trap #15
    ; print new line
    move #13,d0
    lea new_line,a1
    trap #15
    cmpi.b #$79,d1 ; compare with assci of 'y' 
    beq MAINagain
MAINdone
    ; exit program 
    move.l #9,d0
    trap #15
MAINerror_1
    move #13,d0
    lea error_msg1,a1
    trap #15
    jmp MAINagain
MAINClear
    ; clear screen
    move #11,d0
    move.w #$ff00,d1
    trap #15
    jmp MAINloop
    
    
    INCLUDE 'subroutine.x68'
    INCLUDE 'opcode_subroutine.x68'
    INCLUDE 'ea_subroutine.x68'
    INCLUDE 'io_subroutine.x68'
    INCLUDE 'string_subroutine.x68'
    
    INCLUDE 'variables.x68'
    INCLUDE 'strings.x68'

    INCLUDE 'demo_test_hard.x68'
    
    SIMHALT             ; halt simulator

STOP:
    END    MAIN