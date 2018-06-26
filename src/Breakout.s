; Copyright (C) 2018 Georg Ziegler
;
; Permission is hereby granted, free of charge, to any person obtaining a copy of
; this software and associated documentation files (the "Software"), to deal in
; the Software without restriction, including without limitation the rights to
; use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies
; of the Software, and to permit persons to whom the Software is furnished to do
; so, subject to the following conditions:
;
; The above copyright notice and this permission notice shall be included in
; all copies or substantial portions of the Software.
; -----------------------------------------------------------------------------
;   File: Breakout.s
;   Author(s): Georg Ziegler
;   Description: Entry point for Breakout SNES game
;

;-------------------------------------------------------------------------------
;   Includes
;-------------------------------------------------------------------------------
.include "SNESRegisters.inc"
.include "NekoLib.inc"
.include "GameInitialization.inc"
.include "ColorGenerator.inc"
.include "LevelLoader.inc"
.include "Levels.inc"
.include "WRAMPointers.inc"
;-------------------------------------------------------------------------------

;-------------------------------------------------------------------------------
;   Assembler Directives
;-------------------------------------------------------------------------------
.p816
.i16
.a8
;-------------------------------------------------------------------------------

;-------------------------------------------------------------------------------
;   Exports of subroutines for use in other files
;-------------------------------------------------------------------------------
.export     ResetHandler
.export     NMIHandler
.export     IRQHandler
;-------------------------------------------------------------------------------

.segment "CODE"
;-------------------------------------------------------------------------------
;   This is the entry point of the cradle
;-------------------------------------------------------------------------------
.proc   ResetHandler
        SetXY16
        SetA8
        ldx #$1fff              ; set up stack
        txs
        ; force v-blanking, full screen brightness
        lda # ($0f | FORCED_BLANKING_ON)
        sta INIDISP
        stz NMITIMEN            ; disable NMI

        ; clear SNES
        lda #ClearRegistersOpcode ; clear all registers to standard values
        jsl NekoLibLauncher
        lda #ClearVRAMOpcode    ; clear VRAM to zero/$00
        jsl NekoLibLauncher
        lda #ClearCGRAMOpcode   ; clear CG-RAM to zero/$00
        jsl NekoLibLauncher
        phk                     ; restore data bank register
        plb

        ; init Game
        jsl InitGame
        jsl ResetOAMBuffer
        jsl GenerateColors
        jsl InitVariables

        ; load level
        ; tsx                     ; save stack pointer
        ; PushFarAddr OAMBuffer
        ; PushFarAddr Level03Data
        ; jsl LoadLevel
        ; txs                     ; restore stack pointer

        ; make BG1, BG2, and Objects visible
        lda #$13
        sta TM
        ; release forced blanking, full screen brightness
        lda # ($0f | FORCED_BLANKING_OFF)
        sta INIDISP
        ; enable NMI, turn on automatic joypad polling
        lda #$81
        sta NMITIMEN

        nop                     ; break point for debugger

        jml GameLoop
.endproc
;-------------------------------------------------------------------------------

;-------------------------------------------------------------------------------
;   After the ResetHandler will jump to here
;-------------------------------------------------------------------------------
.smart ; keep track of registers widths
.proc   GameLoop
        wai                     ; wait for NMI / V-Blank

        ; react to Input
        ; update background offsets

        jmp GameLoop
.endproc
;-------------------------------------------------------------------------------

;-------------------------------------------------------------------------------
;   Will be called during V-Blank
;-------------------------------------------------------------------------------
.proc   NMIHandler
        lda RDNMI                   ; read NMI status, acknowledge NMI

        ; read input
        lda #PollJoypad1Opcode
        jsl NekoLibLauncher

        ; transfer OAM data
        tsx                         ; save stack pointer
        PushFarAddr OAMBuffer       ; push source address to stack
        lda #UpdateOAMRAMOpcode
        jsl NekoLibLauncher
        txs                         ; restore stack pointer

NMIHandlerDone:
        rti                         ; NMI interrupt done
.endproc
;-------------------------------------------------------------------------------

;-------------------------------------------------------------------------------
;   Is not used in this program
;-------------------------------------------------------------------------------
.proc   IRQHandler
        ; code
        rti
.endproc
;-------------------------------------------------------------------------------
