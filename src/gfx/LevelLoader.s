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
.include "GameConstants.inc"
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
        ; set frame offset to 12: 11 bytes on stack + 1 offset
        FrameOffset = $0b
        LevelDataPointer = FrameOffset
        OAMBufferPointer = FrameOffset + $03

        ; set data bank register
        lda LevelDataPointer + $02, S   ; get bank of level data
        pha                             ; push bank to stack
        plb                             ; set data bank register to bank of level data
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
        xba                     ; else, brick number is color/palette number
        asl                     ; else, brick number is color/palette number
        ora #$2000              ; prio = 2, name = $00
        jmp OAMAttribDone
SolidBrick:
        lda #$3004              ; prio = 3, color = 0, name = $04
OAMAttribDone:
        pha                     ; save OAM attributes on stack

        ; calculate brick position data
        lda #$00                ; clear A
        SetA8                   ; set A to 8-bit
        tya                     ; get the brick counter
        ; calculate horizontal position: HPos = $10 + (brick mod 7) * $20
        ; divide brick number by 7
        sta WRDIVL              ; brick counter is low byte of dividend
        stz WRDIVH              ; high byte of dividend is zero
        lda #$07
        sta WRDIVB              ; divide dividend by divisor $07
        
        nop                     ; wait for 16 cycles
        nop
        nop
        nop

        nop
        nop
        nop
        nop

        lda RDMPYL              ; get remainder
        ShiftALeft $05          ; multiply remainder by $20
        clc                     ; add screen boundry offset...
        adc #LEFT_BOUNDRY
        xba
        lda RDDIVL              ; get quotient
        ShiftALeft $03          ; multiply quotient by $08
        clc                     ; add screen boundry offset...
        adc #$10                ; ...of $10
        xba                     ; final position: VPos in B, HPos in A
        SetA16                  ; set A to 16-bit
        pha                     ; save position data on stack

        ; TODO: Too complicated; calculate address in X and use stack relative addressing
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

        ; correct HPos MSB and size bits
        SetA16                  ; set A to 16-bit
        lda #$aaaa              ; bit mask to store
        ldy #$0200              ; offset into horizontal MSB section of OAM buffer
HMSB:   sta (OAMBufferPointer, S), Y ; store bit mask in OAM buffer
        iny
        iny
        cpy # ($200 + $14)      ; update data for 80 objects
        bcc HMSB
        lda #$caaa              ; HMSB data for objects 80 ~ 83, paddle sprites, and ball
        sta (OAMBufferPointer, S), Y
        SetA8                   ; set A to 8-bit
        ; loading level into OAM buffer done

        pld                     ; restore caller's frame pointer
        RestoreRegisters        ; restore working registers
        rtl
.endproc
;----- end of subroutine GenerateColors ----------------------------------------
