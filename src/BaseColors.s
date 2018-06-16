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
    ; .word   $7fff       ; dummy bytes 
    .word   $001f       ; brick 1 color: red
    .word   $7c00       ; brick 2 color: blue
    .word   $03e0       ; brick 3 color: green
    .word   $7c1f       ; brick 4 color: violet
    .word   $7fe0       ; brick 5 color: light blue
    .word   $03ff       ; ball color: yellow
    .word   $7cf7       ; paddle color: violet
;-------------------------------------------------------------------------------
