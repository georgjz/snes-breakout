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
;   File: GameInitialization.s
;   Author(s): Georg Ziegler
;   Description: This file contains subroutines to initialize the basic game
;   data and variables.
;

;----- Includes ----------------------------------------------------------------
.include "SNESRegisters.inc"
.include "CPUMacros.inc"
.include "WRAMPointers.inc"
.include "MemoryUtils.inc"
.include "GfxData.inc"
;-------------------------------------------------------------------------------

;----- Assembler Directives ----------------------------------------------------
.p816
.i16
.a8
;-------------------------------------------------------------------------------

;-------------------------------------------------------------------------------
;   Routines found in this file
;-------------------------------------------------------------------------------
.export     InitGame            ; Load basic tile sets and map
.export     InitVariables       ; Initialize the variables in WRAM
.export     ResetOAMBuffer      ; Resets the OAM to $ff
;-------------------------------------------------------------------------------

.segment "CODE"
;-------------------------------------------------------------------------------
;   Subroutine: InitGame
;   Parameters: -
;   Description: Load basic tile sets and map
;-------------------------------------------------------------------------------
.proc   InitGame
        PreserveRegisters       ; preserve working registers

        ; load sprite palette into CG-RAM
        tsx                     ; save stack pointer
        PushSizeB $20           ; move a total of 16 bytes/1 palette
        PushSizeB $80           ; CG-RAM destination: $80
        PushFarAddr SpritePalette ; source address for DMA
        jsl LoadPalette         ; call subroutine
        txs                     ; restore stack pointer
        ; sprite palette loaded

        ; load Breakout sprite sheet into VRAM
        tsx                     ; save stack pointer
        PushSizeF $004000       ; size $00:4000
        PushSizeB $00           ; VRAM destination segment: $0000
        PushFarAddr SpriteSheet ; source address for DMA
        jsl LoadTileSet         ; call subroutine
        txs                     ; restore stack pointer
        ; sprite sheet loaded

        ; Set up BG options
        ; set to BG Mode 1, BG2 tile size to 8 x 8 px
        lda # (BG_MODE_1 | BG2_SIZE_8)
        sta BGMODE
        lda #$10                ; set BG2 Base Address
        sta BG12NBA
        ; set BG2 sc address to VRAM address, screen size 32 x 32
        lda # ($00 | BG2_SC_SIZE_32)
        sta BG2SC

        ; set up OBJ options
        ; set OAM Address to $4000, small obj 8x8 px, large 32x32 px
        lda # ($02 | OBJ_SIZE_8_32)
        sta OBJSEL

        RestoreRegisters        ; restore working registers
        rtl
.endproc
;----- end of subroutine InitGame ----------------------------------------------

;-------------------------------------------------------------------------------
;   Subroutine: InitVariables
;   Parameters: -
;   Description: Initialize all variables in WRAM
;-------------------------------------------------------------------------------
.proc   InitVariables
        PreserveRegisters       ; preserve working registers

        ; set start positions

        ; initialize background offsets to zero/$00
        ldx #$00
        stx BG1HOffset
        stx BG1VOffset
        stx BG3HOffset
        stx BG3VOffset
        stx BG2HOffset
        stx BG2VOffset

        RestoreRegisters        ; restore working registers
        rtl
.endproc
;----- end of subroutine InitVariables -----------------------------------------

;-------------------------------------------------------------------------------
;   Subroutine: ResetOAMBuffer
;   Parameters: -
;   Description: Reset OAMBuffer to $ff
;-------------------------------------------------------------------------------
.proc   ResetOAMBuffer
        PreserveRegisters       ; preserve working registers

        ldx #$0000
        lda #$ff
loop:   sta OAMBuffer, x
        inx
        cpx #$0221
        bne loop

        RestoreRegisters        ; restore working registers
        rtl
.endproc
;----- end of subroutine ResetOAMBuffer ----------------------------------------
