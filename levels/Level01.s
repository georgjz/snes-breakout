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
;   File: Level01.lvl
;   Author(s): Georg Ziegler
;   Description: This file contains level one of Breakout
;

.export     Level01Data

.segment "LEVELS": far
;-------------------------------------------------------------------------------
;   Level 01 Data
;-------------------------------------------------------------------------------
Level01Data:
.byte   0, 1, 2, 1, 2, 1, 0     ; row 1
.byte   0, 1, 0, 1, 0, 1, 0
.byte   0, 1, 0, 1, 0, 1, 0
.byte   0, 1, 0, 1, 0, 1, 0
.byte   0, 1, 0, 1, 0, 1, 0
.byte   0, 1, 0, 1, 0, 1, 0
.byte   0, 1, 0, 1, 0, 1, 0
.byte   0, 1, 0, 1, 0, 1, 0
.byte   0, 1, 0, 1, 0, 1, 0
.byte   0, 1, 0, 1, 0, 1, 0
.byte   0, 1, 0, 1, 0, 1, 0
.byte   0, 1, 0, 1, 0, 1, 0
;-------------------------------------------------------------------------------
