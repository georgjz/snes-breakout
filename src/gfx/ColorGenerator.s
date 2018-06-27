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

;-------------------------------------------------------------------------------
;   Includes
;-------------------------------------------------------------------------------
.include "SNESRegisters.inc"
.include "NekoLib.inc"
.include "BaseColors.inc"
.include "GfxData.inc"
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
.export     GenerateColors      ; Generates the colors/palettes for all objects
;-------------------------------------------------------------------------------

.segment "CODE"
;-------------------------------------------------------------------------------
;   Subroutine: GenerateColors
;   Parameters: -
;   Description: Generates color palettes based on the base colors set in BaseColors.s
;-------------------------------------------------------------------------------
.proc   GenerateColors
        PreserveRegisters       ; preserve working registers
        phb                     ; preserve data bank register
        phd                     ; preserve callers frame pointer
        tsc                     ; make own frame pointer in D
        tcd
        FrameOffset = $0b       ; set frame offset to 11: 10 bytes on stack + 1 offset
        ldx #$00                ; X is used as argument offset

        ; TODO: Potential for cycle golfing with bitmasks
        ; blend the base color with the shade from the grayscale
        ; A - base color, later blended color
        ; X - current color
        ; Y - current palette/base color
        ; final colors/palettes are stored on stack and transfered by DMA to CG-RAM
        phk                     ; set data bank register...
        plb                     ; ...to current bank address
        SetA16                  ; set A to 16-bit
        ldy #$00                ; palette counter/offset
PaletteLoop:
        ldx #$1e                ; reset color counter
        ; trans color?
ColorLoop:
        lda #$0000              ; clear A
        pha                     ; push new color to stack/increase stack pointer
        ; txs/inc combo faster?
        ; calculate R component of blended color
        lda SpritePalette, X    ; get grayscale shade
        and #$001f              ; mask R component
        xba                     ; move gray R value to higher byte of A
        pha                     ; save gray R on stack
        lda BaseColorTable, Y   ; get base color
        and #$001f              ; mask R component
        ora $01, S              ; load gray R component into higher byte
        sta WRMPYA              ; multiply high and low byte
        pla ;nop                ; wait for 2..., use to decrease stack pointer
        nop                     ; ...4...
        nop                     ; ...6...
        nop                     ; ...8 cycles
        lda RDMPYL              ; get multiplication result
        ShiftARight $05         ; divide result by $20
        sta $01, S              ; store R component

        ; calculate G component of blended color
        lda SpritePalette, X    ; get grayscale shade
        and #$03e0              ; mask R component
        ShiftARight $05         ; shift right 5 times
        xba                     ; move gray G value to higher byte of A
        pha                     ; save gray G value on stack
        lda BaseColorTable, Y   ; get base color
        and #$03e0              ; mask G component
        ShiftARight $05         ; shift right 5 times
        ora $01, S              ; load gray G component into higher byte
        sta WRMPYA              ; multiply high and low byte
        pla ;nop                ; wait for 2..., decrease stack pointer
        nop                     ; ...4...
        nop                     ; ...6...
        nop                     ; ...8 cycles
        lda RDMPYL              ; get multiplication result
        ; ShiftARight $05         ; divide result by $20
        and #$03e0              ; clear lower byte of result
        ; ShiftALeft $05          ; shift left 5 times
        ora $01, S              ; mix G and R component
        sta $01, S              ; store GR component

        ; calculate B component of blended color
        lda SpritePalette, X    ; get grayscale shade
        and #$7f00              ; mask B component
        ShiftARight $0a         ; shift right 10 times
        xba                     ; move gray B value to higher byte of A
        pha                     ; save gray B value on stack
        lda BaseColorTable, Y   ; get base color
        and #$7f00              ; mask B component
        ShiftARight $0a         ; shift right 10 times
        ora $01, S              ; load gray B component into higher byte
        sta WRMPYA              ; multiply high and low byte
        pla ;nop                ; wait for 2..., decrease stack pointer
        nop                     ; ...4...
        nop                     ; ...6...
        nop                     ; ...8 cycles
        lda RDMPYL              ; get multiplication result
        ; ShiftARight $05         ; divide result by $20
        and #$03e0              ; isolate 5-bit result
        ShiftALeft $05          ; shift left 5 times
        ora $01, S              ; mix B, G and R component
        sta $01, S              ; store BGR555 color on stack
        ; the blended color is now stored on stack
        nop
        ; check loop condition
        dex                     ; decrement X by 2
        dex
        cpx #$00                ; check if all colors done
        bmi :+                  ; if X < 0, go to next palette
        jmp ColorLoop           ; else calculate next color
:       iny                     ; move to next base color...
        iny                     ; ...by incrementing Y by 2
        cpy #$0d                ; check if all base colors done
        bcs :+
        jmp PaletteLoop
:       ; generated palettes are now on the stack

        ; move generated palettes from stack to CG-RAM with DMA. The inital
        ; stack pointer saved in X now points to the first byte of the first palette
        SetA8
        tsx                     ; get stack pointer
        PushSizeB $e0           ; move 7 palettes to CG-RAM
        PushSizeB $90           ; move to sprite palette starting at palette 1
        PushSizeB $00           ; bank of source address, zero since stack is on ZP
        inx                     ; increase source address by one, since it is the stack pointer
        phx                     ; high and low bytes of source address
        lda #LoadPaletteOpcode
        jsl NekoLibLauncher     ; DMA generated palettes to CG-RAM

        ; restore stack pointer to old value
        tsc                     ; load stack pointer into A
        clc                     ; add palette offset...
        adc #$e5                ; ...to A
        tcs                     ; stack pointer now on last byte of palettes/colors

        ; SetA8                   ; set A back to 8-bit
        pld                     ; restore caller's frame pointer
        plb                     ; restore caller's data bank register
        RestoreRegisters        ; restore working registers
        rtl
.endproc
;----- end of subroutine GenerateColors ----------------------------------------
