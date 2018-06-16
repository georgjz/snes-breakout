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
;   File: BrickStruct.inc
;   Author(s): Georg Ziegler
;   Description: This file contains the definition of a struct that represents
;   a single brick on the playing field
;

.ifndef BRICKSTRUCT_INC
.define BRICKSTRUCT_INC

;-------------------------------------------------------------------------------
;   Constants for use in logical expressions
;-------------------------------------------------------------------------------
DESTROYED   = $02
IS_SOLID    = $01
;-------------------------------------------------------------------------------

;-------------------------------------------------------------------------------
;   Data structure for keeping track of the state of a brick
;-------------------------------------------------------------------------------
.struct     Brick
            HPos            .byte       ; horizontal position
            VPos            .byte       ; vertical position
            State           .byte       ; Standing and facing direction
.endstruct
;-------------------------------------------------------------------------------

.endif  ; BRICKSTRUCT_INC
