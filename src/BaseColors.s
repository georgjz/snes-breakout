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
;   File: WRAMPointers.s
;   Author(s): Georg Ziegler
;   Description: This file contains the base colors for the game objects
;

;-------------------------------------------------------------------------------
;   Pointers found in this file
;-------------------------------------------------------------------------------
.export     BaseColorTable  ; Pointer to base colors
;-------------------------------------------------------------------------------

.segment "CODE"
;-------------------------------------------------------------------------------
;   Base Colors
;-------------------------------------------------------------------------------
BaseColorTable:
    .word   $001f       ; base color for sprite palette 7
    .word   $7c00       ; base color for sprite palette 6
    .word   $03e0       ; base color for sprite palette 5
    .word   $7c1f       ; base color for sprite palette 4
    .word   $7fe0       ; base color for sprite palette 3
    .word   $03ff       ; base color for sprite palette 2
    .word   $7cf7       ; base color for sprite palette 1
    ; sprite palette 0 is the grayscale palette reserved for solid bricks
;-------------------------------------------------------------------------------
