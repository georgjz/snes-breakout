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
.include "GameConstants.inc"
.include "GfxData.inc"
.include "MemoryMap.inc"
.include "ObjStruct.inc"
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
;   Routines found in this file
;-------------------------------------------------------------------------------
.export     InitGame            ; Load basic tile sets and maps
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

        tsx                         ; save stack pointer
        ; load background palette into CG-RAM
        PushSizeB $20               ; move a total of 32 bytes/1 palette
        PushSizeB $00               ; CG-RAM destination: $00
        PushFarAddr SpritePalette   ; source address for DMA
        lda #LoadPaletteOpcode
        jsl NekoLibLauncher         ; call subroutine
        ; load sprite palette into CG-RAM
        txs                         ; restore stack pointer
        PushSizeB $20               ; move a total of 32 bytes/1 palette
        PushSizeB $80               ; CG-RAM destination: $80
        PushFarAddr SpritePalette   ; source address for DMA
        lda #LoadPaletteOpcode
        jsl NekoLibLauncher         ; call subroutine
        txs                         ; restore stack pointer
        ; sprite palette loaded

        ; load Breakout sprite sheet into VRAM
        PushSizeF $004000           ; size $00:4000
        PushSizeB SPRITE_DATA_SEG   ; VRAM destination segment: $0000
        PushFarAddr SpriteSheet     ; source address for DMA
        lda #LoadTileSetOpcode
        jsl NekoLibLauncher         ; call subroutine
        txs                         ; restore stack pointer
        ; sprite sheet loaded

        ; load tilemaps into VRAM
        ; game border map
        PushSizeF $000800           ; size: $00:0800, 2KB
        PushSizeB BORDER_MAP_SEG    ; destination address: segment $08 = $4000
        PushFarAddr GameBorderMap   ; origin address
        lda #LoadTileMapOpcode
        jsl NekoLibLauncher         ; load tilemap
        txs                         ; restore stack pointer
        ; opaque screen mask
        PushSizeF $000800           ; size: $00:0800, 2KB
        PushSizeB OPAQUE_MAP_SEG    ; destination address: segment $09 = $4800
        PushFarAddr OpaqueMap       ; origin address
        lda #LoadTileMapOpcode
        jsl NekoLibLauncher         ; load tilemap
        txs                         ; restore stack pointer
        ; splash screen map
        PushSizeF $000800           ; size: $00:0800, 2KB
        PushSizeB SPLASH_MAP_SEG    ; destination address: segment $0a = $5000
        PushFarAddr SplashMap       ; origin address
        lda #LoadTileMapOpcode
        jsl NekoLibLauncher         ; load tilemap
        txs                         ; restore stack pointer
        ; start menu map
        PushSizeF $000800           ; size: $00:0800, 2KB
        PushSizeB START_MENU_SEG    ; destination address: segment $0c = $6000
        PushFarAddr StartMenuMap    ; origin address
        lda #LoadTileMapOpcode
        jsl NekoLibLauncher         ; load tilemap
        txs                         ; restore stack pointer
        ; tilemaps loaded into VRAM

        ; Set up BG options
        ; set to BG Mode 1, all three BGs tile size to 16 x 16 px, BG3 prio = 1
        lda # (BG_MODE_1 | BG1_SIZE_16 | BG2_SIZE_16 | BG3_SIZE_16 | BG3_PRIO_ON)
        sta BGMODE
        lda #$00                ; set BG1, BG2, and BG3 Base Address to $0000
        sta BG12NBA
        sta BG34NBA
        ; set tilemap addresses
        lda # (SPLASH_MAP_SEG << 2 | BG1_SC_SIZE_32)    ; BG1: hold the splash screen
        sta BG1SC
        lda # (OPAQUE_MAP_SEG << 2 | BG2_SC_SIZE_32)    ; BG2: hold the opaque screen
        sta BG2SC
        lda # (OPAQUE_MAP_SEG << 2 | BG3_SC_SIZE_32)    ; BG3: menu, empty for now
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

        ; symbols used for the start position of paddle and ball used to reset
        ; them before each level
        ; PaddleStartHPos = $6c
        ; PaddleStartVPos = $d0

        ; set inital paddle data
        lda #PADDLE_START_HPOS  ; set horizontal position
        sta Paddle+ObjData::HPos
        lda #PADDLE_START_VPOS  ; set vertical position
        sta Paddle+ObjData::VPos
        lda #$02                ; set horizontal speed
        sta Paddle+ObjData::HSpeed
        lda #$00                ; set vertical speed
        sta Paddle+ObjData::VSpeed
        lda #$28                ; set horizontal size
        sta Paddle+ObjData::HSize
        lda #$08                ; set vertical size
        sta Paddle+ObjData::VSize

        ; set inital data for ball
        lda # (PADDLE_START_HPOS + $10) ; set horizontal position
        sta Ball+ObjData::HPos
        lda # (PADDLE_START_VPOS - $08) ; set vertical position
        sta Ball+ObjData::VPos
        lda #INITIAL_BALL_HSPEED ; set horizontal speed to 2
        sta Ball+ObjData::HSpeed
        lda #INITIAL_BALL_VSPEED ; set vertical speed to -2
        sta Ball+ObjData::VSpeed
        lda #$08                ; set horizontal size
        sta Ball+ObjData::HSize
        lda #$08                ; set vertical size
        sta Ball+ObjData::VSize
        lda #$01                ; set the ball to sticky
        sta BallSticky

        ; set OAM data for paddle and ball
        ; lda OAMBuffer + HMSB_OAM_OFFSET + $14
        ; SetA16
        ; ldx #$150
        ; lda Paddle+ObjData::HPos
        ; sta OAMBuffer, X        ; store position data in OAM buffer
        ; inx
        ; inx
        ; lda #$3e40
        ; sta OAMBuffer, X        ; store OAM attribute data
        ; SetA8

        ; initialize background offsets to zero/$00
        ldx #$00
        stx BG1HOffset
        stx BG1VOffset
        stx BG3HOffset
        stx BG3VOffset
        stx BG2HOffset
        stx BG2VOffset

        ; set vertical offset of background 1 for initial intro scrolling
        ldx #$00a0              ; offset 10 * 16 = $a0
        stx BG1VOffset          ; store new offset
        lda BG1VOffset          ; get lower byte of vertical offset
        sta BG1VOFS             ; set BG1 vertical offset, lower byte
        lda BG1VOffset + $01    ; get high byte of vertical offset
        and #$1f                ; clear upper 3 bits
        sta BG1VOFS             ; set BG1 vertical offset, bits 8 ~ 12

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

        ldx #$0000              ; init counter to zero
        lda #$01
loop1:  sta OAMBuffer, x
        inx
        cpx #$0221
        bne loop1

        ldx #$0000
        lda #$ff                ; set horizontal position MSB to 1 to move all sprites offscreen
loop2:  sta OAMBuffer + $200, x
        inx
        cpx #$20
        bne loop2

        RestoreRegisters        ; restore working registers
        rtl
.endproc
;----- end of subroutine ResetOAMBuffer ----------------------------------------
