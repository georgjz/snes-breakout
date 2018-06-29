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
;   File: ObjStruct.inc
;   Author(s): Georg Ziegler
;   Description: This file contains the definition of a struct that holds the
;   paddle and the ball
;

.ifndef OBJECTSTRUCT_INC
.define OBJECTSTRUCT_INC

;-------------------------------------------------------------------------------
;   Data structure for keeping track of the state of the paddle and the ball
;-------------------------------------------------------------------------------
.struct     ObjData
            HPos            .byte       ; horizontal position
            VPos            .byte       ; vertical position
            HSpeed          .byte       ; horizontal speed
            VSpeed          .byte       ; vertical speed
            HSize           .byte       ; horizontal size
            VSize           .byte       ; vertical size
.endstruct
;-------------------------------------------------------------------------------

.endif  ; OBJECTSTRUCT_INC
