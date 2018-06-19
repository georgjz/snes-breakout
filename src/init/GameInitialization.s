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

;-------------------------------------------------------------------------------
;   Includes
;-------------------------------------------------------------------------------
.include "SNESRegisters.inc"
.include "NekoLib.inc"
.include "WRAMPointers.inc"
.include "GfxData.inc"
;-------------------------------------------------------------------------------

;-------------------------------------------------------------------------------
;   Assembler Directives
;-------------------------------------------------------------------------------
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

        tsx                     ; save stack pointer
        ; load background palette into CG-RAM
        PushSizeB $20           ; move a total of 32 bytes/1 palette
        PushSizeB $00           ; CG-RAM destination: $00
        PushFarAddr SpritePalette ; source address for DMA
        lda #LoadPaletteOpcode
        jsl NekoLibLauncher     ; call subroutine
        ; load sprite palette into CG-RAM
        txs                     ; restore stack pointer
        PushSizeB $20           ; move a total of 32 bytes/1 palette
        PushSizeB $80           ; CG-RAM destination: $80
        PushFarAddr SpritePalette ; source address for DMA
        lda #LoadPaletteOpcode
        jsl NekoLibLauncher     ; call subroutine
        txs                     ; restore stack pointer
        ; sprite palette loaded

        ; load Breakout sprite sheet into VRAM
        PushSizeF $004000       ; size $00:4000
        PushSizeB $00           ; VRAM destination segment: $0000
        PushFarAddr SpriteSheet ; source address for DMA
        lda #LoadTileSetOpcode
        jsl NekoLibLauncher     ; call subroutine
        txs                     ; restore stack pointer
        ; sprite sheet loaded

        ; load tilemaps into VRAM
        PushSizeF $000800       ; size: $00:0800, 2KB
        PushSizeB $08           ; destination address: segment $08 = $4000
        PushFarAddr BG1Map      ; origin address
        lda #LoadTileMapOpcode
        jsl NekoLibLauncher     ; load tilemap
        txs                     ; restore stack pointer
        PushSizeF $000800       ; size: $00:0800, 2KB
        PushSizeB $09           ; destination address: segment $09 = $4800
        PushFarAddr BG2Map      ; origin address
        lda #LoadTileMapOpcode
        jsl NekoLibLauncher     ; load tilemap
        txs                     ; restore stack pointer
        PushSizeF $000800       ; size: $00:0800, 2KB
        PushSizeB $0a           ; destination address: segment $0a = $5000
        PushFarAddr BG3Map      ; origin address
        lda #LoadTileMapOpcode
        jsl NekoLibLauncher     ; load tilemap
        txs                     ; restore stack pointer
        ; tilemaps loaded into VRAM

        ; Set up BG options
        ; set to BG Mode 1, all three BGs tile size to 16 x 16 px, BG3 prio = 1
        lda # (BG_MODE_1 | BG1_SIZE_16 | BG2_SIZE_16 | BG3_SIZE_16 | BG3_PRIO_ON)
        sta BGMODE
        lda #$00                ; set BG1, BG2, and BG3 Base Address to $0000
        sta BG12NBA
        sta BG34NBA
        ; set tilemap addresses
        lda # ($20 | BG1_SC_SIZE_32)    ; BG1: $4000
        sta BG1SC
        lda # ($24 | BG2_SC_SIZE_32)    ; BG2: $4800
        sta BG2SC
        lda # ($28 | BG3_SC_SIZE_32)    ; BG3: $5000
        sta BG3SC

        ; set up OBJ options
        ; set OAM Address to $0000, small obj 8x8 px, large 32x32 px
        lda # ($00 | OBJ_SIZE_8_32)
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
