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
        ; set frame offset to 13: 10 bytes on stack + 1 offset + 2 1-byte variables
        FrameOffset = $0d
        ldx #$00                ; X is used as argument offset

        ; CODE
        ; for column < 12
        ;   for row < 7
        ;       read brick color and type
        ;       brick.HPos = $10 + row * $20
        ;       brick.VPos = $10 + column * $08
        ;       add new brick to OAM buffer:
        ;           .word position = brick.VPos << 8 + brick.HPos
        ;           .word attrib = % 00vs'cccn'nnnn'nnnn
        ;               if brick.type == zero
        ;                   v = 0
        ;               else
        ;                   v = 1
        ;               if brick.type == solid | zero
        ;                   s = 1
        ;               else
        ;                   s = 0
        ;               ccc = brick.color
        ;               if s == 1
        ;                   name = $04
        ;               else
        ;                   name = $00
        ;
Columns:
Rows:
RowDone:

        pld                     ; restore caller's frame pointer
        RestoreRegisters        ; restore working registers
        rtl
.endproc
;----- end of subroutine GenerateColors ----------------------------------------
