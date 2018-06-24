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
        ; phb                     ; preserve data bank register
        tsc                     ; make own frame pointer in D
        tdc
        ; set frame offset to 12: 11 bytes on stack + 1 offset
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
BrickLoop:
        ; tsx                     ; save stack pointer in X
        ; calculate OAM attribute data
        ; SetA8                   ; set A to 8-bit
        lda (LevelDataPointer, S), Y ; load brick number
        SetA16                  ; set A to 16-bit
        and #$00ff              ; clear B
        beq OAMAttribDone       ; if brick number zero, i.e, empty
        cmp #$01                ; if brick is solid...
        beq SolidBrick          ; ...create a solid brick
        ShiftALeft $05          ; else, brick number is color/palette number
        ora #$2004              ; prio = 2, name = $04
        jmp OAMAttribDone
SolidBrick:
        lda #$3000              ; prio = 3, color = 0, name = 0
OAMAttribDone:
        pha                     ; save OAM attributes on stack

        ; calculate brick position data
        lda #$00                ; clear A
        SetA8                   ; set A to 8-bit
        tya                     ; get the brick counter
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
        ply                     ; pull brick counter back into Y
        SetA16                  ; set A to 16-bit
        pha                     ; save position data on stack

        ; Calculate the destination address and store position and attribute data in OAM buffer
        ; destination address = (OAMBufferPointer) + 4 * Y
        tya                     ; move brick counter to A
        ShiftALeft $02          ; multiply brick counter by 4
        tay                     ; move brick counter back to Y
        pla                     ; pull position data from stack
        plx                     ; pull OAM attribute data from stack, necessary for correct stack pointer
        sta (OAMBufferPointer, S), Y ; store position data in OAM buffer
        iny                     ; increment offset by 2
        iny
        txa                     ; transfer attribute data to A
        sta (OAMBufferPointer, S), Y ; store attribute data in OAM buffer
        tya                     ; move brick counter to A
        dec                     ; decrement offset by 2
        dec
        ShiftARight $02         ; divide counter by 4
        tay                     ; move brick counter back to Y
        ; check brick counter
        iny
        cpy # ($07 * $0c)
        ; bcc :+
        bcc BrickLoop
        ; jmp BrickLoop
; :       ; loading level into OAM buffer done

        ; phk                     ; restore data bank register
        ; plb
        pld                     ; restore caller's frame pointer
        SetA8                   ; set A to 8-bit
        RestoreRegisters        ; restore working registers
        rtl
.endproc
;----- end of subroutine GenerateColors ----------------------------------------
