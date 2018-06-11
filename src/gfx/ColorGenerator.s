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
;   File: ColorGenerator.s
;   Author(s): Georg Ziegler
;   Description: This file contains subroutines to dynamically generate palettes
;   that can be loaded into CG-RAM
;

;----- Includes ----------------------------------------------------------------
.include "SNESRegisters.inc"
.include "CPUMacros.inc"
.include "WRAMPointers.inc"
.include "BaseColors.inc"
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
.export     GenerateColors      ; Generates the colors/palettes for all objects
;-------------------------------------------------------------------------------

.segment "CODE"
;-------------------------------------------------------------------------------
;   Subroutine: GenerateColors
;   Parameters: -
;   Description: Generates color palettes based on the base colors set in BaseColors.inc
;-------------------------------------------------------------------------------
.proc   GenerateColors
        PreserveRegisters       ; preserve working registers
        phd                     ; preserve callers frame pointer
        tsc                     ; make own frame pointer in D
        tcd
        FrameOffset = $0b       ; set frame offset to 11: 10 bytes on stack + 1 offset
        ldx #$00                ; X is used as argument offset

        ; blend the base color with the shade from the grayscale
        ; A - base color
        ; Y - current color table offset
        ; final colors/palettes are stored on stack and transfered by DMA to CG-RAM
        phk                     ; set data bank register...
        plb                     ; ...to current bank address
        SetA16                  ; set A to 16-bit
        ldx #$02                ; palette counter
        ldy #$00                ; set offset to $02, which will skip the trans color
PaletteLoop:
        PushSizeW $3c00         ; push trans color
ColorLoop:
        lda #$0000              ; clear A
        pha                     ; push new color to stack
        lda BaseColorTable, Y   ; get base color
        and #$001f              ; extract R
        clc
        adc SpritePalette, X    ; add grayscale shade to R
        asl                     ; divide by 2
        and #$001f              ; clear B and G
        sta $01, S              ; store R in new color
        ; repeate for green
        lda BaseColorTable, Y   ; get base color
        and #$03e0              ; extract G
        clc
        adc SpritePalette, X    ; add grayscale shade to G
        asl                     ; divide by 2
        and #$03e0              ; clear B and R
        ora $01, S              ; add G to new color...
        sta $01, S              ; ...and store it
        ; repeate for blue
        lda BaseColorTable, Y   ; get base color
        and #$7c00              ; extract B
        clc
        adc SpritePalette, X    ; add grayscale shade to G
        asl                     ; divide by 2
        and #$7c00              ; clear B and R
        ora $01, S              ; add B to new color...
        and #$7fff              ; clear MSB
        sta $01, S              ; ...and store it
        inx                     ; increment Y by 2
        inx
        cpx #$20                ; check if all colors done
        bcc ColorLoop
        iny                     ; move to next base color...
        iny                     ; ...by incrementing X by 2
        ldx #$0002              ; reset color offset
        cpy #$0d                ; check if all base colors done
        bcc PaletteLoop
        ; generated palettes are now on the stack

        ; move generated palettes from stack to CG-RAM with DMA. The inital
        ; stack pointer saved in X now points to the low byte of the first palette
        SetA8
        PushSizeB $e0           ; move 7 palettes to CG-RAM
        PushSizeB $90           ; move to sprite palette starting at palette 1
        tsx                     ; get stack pointer
        PushSizeB $00           ; bank of source address
        phx                     ; high and low bytes of source address
        ; lda #$00              ; reset A
        jsl LoadPalette         ; DMA generated palettes to CG-RAM
        ; txs                     ; restore old stack pointer

        ; restore stack pointer to old value
        tsc
        clc
        adc #$e5
        tcs

        ; SetA8                   ; set A back to 8-bit
        pld                     ; restore callers frame pointer
        RestoreRegisters        ; restore working registers
        rtl
.endproc
;----- end of subroutine GenerateColors ----------------------------------------
