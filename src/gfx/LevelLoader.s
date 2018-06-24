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
.include "WRAMPointers.inc"
.include "BaseColors.inc"
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
.export     LoadLevel           ; Reads a level file into OAM buffer
;-------------------------------------------------------------------------------

.segment "CODE"
;-------------------------------------------------------------------------------
;   Subroutine: LoadLevel
;   Parameters: .far LevelData, .far OAMBuffer
;   Description: Reads a level file and stores the brick data in OAMBuffer
;-------------------------------------------------------------------------------
.proc   LoadLevel
        PreserveRegisters       ; preserve working registers
        phd                     ; preserve callers frame pointer
        tsc                     ; make own frame pointer in D
        tdc
        ; create 2 local variables on stack
        ; PushSizeB $00
        ; PushSizeB $00
        ; row = $01
        ; column = $02
        ; set frame offset to 11: 10 bytes on stack + 1 offset
        FrameOffset = $0b
        LevelDataPointer = FrameOffset
        OAMBufferPointer = FrameOffset + $03
        ldx #$00                ; X is used as argument offset

        ; CODE
        ; for brick < 12 * 7
        ;   read brick color and type
        ;   brick.HPos = $10 + (brick mod 7) * $20
        ;   brick.VPos = $10 + (brick mod 8) * $08
        ;   add new brick to OAM buffer:
        ;       .word position = brick.VPos << 8 + brick.HPos
        ;       .word attrib = % 00vs'cccn'nnnn'nnnn
        ;           if brick.type == zero
        ;               v = 0
        ;           else
        ;               v = 1
        ;           if brick.type == solid | zero
        ;               s = 1
        ;           else
        ;               s = 0
        ;           ccc = brick.color
        ;           if s == 1
        ;               name = $04
        ;           else
        ;               name = $00
        ;

        ; set data bank register
        lda LevelDataPointer + $02, S
        pha
        plb
        ; A - calculated data, position and OAM attributes
        ; X - auxiliar for stack operations
        ; Y - current brick counter, auxiliar for division
        ldy #$0000              ; Y will serve as brick counter
Loop:   tya                     ; get the brick number
        ; calculate horizontal position: HPos = $10 + (brick mod 7) * $20
        ; divide brick number by 7
        phy                     ; save current brick number on stack
        pha                     ; push numerator to stack
        lda #$07                ; load denominator...
        pha                     ; ...and push to stack
        lda #$00                ; will hold remainder (= brick number mod 7)
        ldy #$07                ; bit counter
        tsx                     ; use X as offset to access numerator and denominator in zero page/stack
        ; TODO: Replace with SNES division, saves a LOT of cycles
        clc
:       rol $02, X
        rol
        cmp $01, X
        bcc :+
        sbc $01, X
:       dey
        bpl :--
        rol $02, X
        ShiftALeft $05          ; multiply remainder by $20
        clc                     ; add screen boundry offset...
        adc #$10                ; ...of $10
        xba                     ; save horizontal position in B
        ; calculate vertical position: VPos = $10 + (brick / 7) * $08
        pla                     ; pull denominator
        pla                     ; pull quotient
        ShiftALeft $03          ; multiply quotient by $08
        clc                     ; add screen boundry offset...
        adc #$10                ; ...of $10
        xba                     ; final position: VPos in B, HPos in A
        ply                     ; restore brick counter
        ; BUG: Address must increase by 4
        SetA16                  ; set A to 16-bit
        sta (OAMBufferPointer, S), Y ; store position data in OAM buffer
        SetA8                   ; set A to 8-bit
        ; check brick counter
        iny
        cpy # ($07 * $0c)
        bcc Loop

        phk                     ; restore data bank register
        plb
        pld                     ; restore caller's frame pointer
        RestoreRegisters        ; restore working registers
        rtl
.endproc
;----- end of subroutine GenerateColors ----------------------------------------
