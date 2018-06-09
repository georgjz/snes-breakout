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
;   File: TileData.s
;   Author(s): Georg Ziegler
;   Description: This file places the graphics in the ROM
;

;----- Assembler Directives ----------------------------------------------------
.p816
.i16
.a8
;-------------------------------------------------------------------------------

;-------------------------------------------------------------------------------
;   Pointers found in this file
;-------------------------------------------------------------------------------
.export     SpriteSheet
.export     SpritePalette
;-------------------------------------------------------------------------------

;-------------------------------------------------------------------------------
;   Pointers to graphics in ROM
;-------------------------------------------------------------------------------
.segment "SPRITEDATA"
SpriteSheet:    .incbin "SpriteSheet.vra"
SpritePalette:  .incbin "SpritePalette.pal"
;-------------------------------------------------------------------------------
