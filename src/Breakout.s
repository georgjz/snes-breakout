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
.include "GameState.inc"
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

        ; init game data
        jsl InitGame
        jsl ResetOAMBuffer
        jsl GenerateColors
        jsl InitVariables
        phk                     ; restore data bank register
        plb

        ; intro
        lda #GAME_STATE_FADE    ; set game state to fading
        sta GameState
        lda #$03                ; make BG1, BG2, and OBJs visible
        sta TM
        lda # (FORCED_BLANKING_OFF | $00) ; turn off forced blanking, screen brightness to zero
        sta INIDISP
        lda #$81                ; enable NMI
        sta NMITIMEN

        ; intro fade
        lda #$00                ; push inital screen brightness to stack
        pha
        tsx                     ; store stack pointer in X to use as offset
FadeLoop:
        inc $01, X              ; increment screen brightness by 1
        wai                     ; wait for NMI interrupt
        lda $01, X              ; get current screen brightness
        cmp #$0f                ; check if max brightness...
        bcs FadeLoopDone        ; ...then fade loop done
        ora FORCED_BLANKING_OFF ; set blanking bit
        sta INIDISP             ; set new screen brightness
        jmp FadeLoop            ; redo loop
FadeLoopDone:
        pla                     ; reset stack pointer

        ; intro scroll
ScrollLoop:
        wai                     ; wait for NMI
        SetA16
        lda BG1VOffset          ; get current vertical offset
        sec                     ; decrement offset...
        sbc #$01                ; ...by 1
        beq ScrollLoopDone      ; if offset down to zero, scrolling is done
        sta BG1VOffset          ; save new offset
        SetA8
        sta BG1VOFS             ; set lower byte of new offset
        xba                     ; get higher byte of offset
        and #$1f                ; clear upper 3 bits
        sta BG1VOFS             ; set higher byte of new offset
        jmp ScrollLoop          ; redo loop
ScrollLoopDone:
        sta BG1VOffset          ; save new offset
        SetA8
        ;   get current offset
        ;   decrement offset
        ;
        lda #GAME_STATE_MENU    ; change game state to menu
        sta GameState

        nop                     ; break point for debugger

        jml GameLoop
.endproc
;-------------------------------------------------------------------------------

;-------------------------------------------------------------------------------
;   After the ResetHandler will jump to here
;-------------------------------------------------------------------------------
.proc   GameLoop
        wai                     ; wait for NMI / V-Blank

        ; react to Input
        ; if game state = menu
        ;   if start button pressed
        ;       fade out
        ;       turn off NMI, forced blanking on
        ;       load level
        ;       turn on NMI, forced blanking off
        ;       fade in
        ; update background offsets

        ; if game state = run
        ;   update Paddle
        ;   update Ball
        ;       do collision checks


GameLoopDone:
        jmp GameLoop
.endproc
;-------------------------------------------------------------------------------

;-------------------------------------------------------------------------------
;   Will be called during V-Blank
;-------------------------------------------------------------------------------
.proc   NMIHandler
        lda RDNMI                   ; read NMI status, acknowledge NMI

        ; check game state
        lda GameState               ; get current game state
        cmp #GAME_STATE_FADE        ; if game screen is fading...
        beq NMIHandlerDone          ; ...skip all calls in NMI

        ; read input
        tsx                         ; save stack pointer
        PushFarAddr Joy1Raw
        lda #PollJoypad1Opcode
        jsl NekoLibLauncher

        ; transfer OAM data
        txs                         ; restore stack pointer
        PushFarAddr OAMBuffer       ; push source address to stack
        lda #UpdateOAMRAMOpcode
        jsl NekoLibLauncher
        txs                         ; restore stack pointer

NMIHandlerDone:
        rti                         ; NMI interrupt done
.endproc
;-------------------------------------------------------------------------------

;-------------------------------------------------------------------------------
;   This is not used in this program
;-------------------------------------------------------------------------------
.proc   IRQHandler
        ; code
        rti
.endproc
;-------------------------------------------------------------------------------
