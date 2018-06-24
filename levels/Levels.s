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
;   File: Levels.lvl
;   Author(s): Georg Ziegler
;   Description: This file contains levels for Breakout
;

;-------------------------------------------------------------------------------
;   Levels found in this file
;-------------------------------------------------------------------------------
.export     Level01Data
.export     Level02Data
.export     Level03Data
.export     Level04Data
.export     Level05Data
;-------------------------------------------------------------------------------

.segment "LEVELS": far
;-------------------------------------------------------------------------------
;   Level 01 Data
;-------------------------------------------------------------------------------
Level01Data:
.byte   5, 5, 5, 5, 5, 5, 5     ; row 1
.byte   5, 5, 5, 5, 5, 5, 5
.byte   7, 7, 7, 7, 7, 7, 7
.byte   7, 7, 7, 7, 7, 7, 7
.byte   4, 1, 4, 1, 4, 1, 4
.byte   4, 1, 4, 1, 4, 1, 4
.byte   3, 3, 3, 3, 3, 3, 3
.byte   3, 3, 3, 3, 3, 3, 3
.byte   2, 2, 2, 2, 2, 2, 2
.byte   2, 2, 2, 2, 2, 2, 2
.byte   6, 6, 6, 6, 6, 6, 6
.byte   6, 6, 6, 6, 6, 6, 6
;-------------------------------------------------------------------------------

;-------------------------------------------------------------------------------
;   Level 02 Data
;-------------------------------------------------------------------------------
Level02Data:
.byte   2, 2, 2, 2, 2, 2, 2     ; row 1
.byte   2, 1, 2, 1, 2, 1, 2
.byte   3, 3, 3, 3, 3, 3, 3
.byte   3, 3, 3, 3, 3, 3, 3
.byte   4, 1, 4, 1, 4, 1, 4
.byte   4, 4, 4, 4, 4, 4, 4
.byte   5, 5, 5, 5, 5, 5, 5
.byte   5, 1, 5, 1, 5, 1, 5
.byte   6, 6, 6, 6, 6, 6, 6
.byte   6, 6, 6, 6, 6, 6, 6
.byte   7, 1, 7, 1, 7, 1, 7
.byte   7, 7, 7, 7, 7, 7, 7
;-------------------------------------------------------------------------------

;-------------------------------------------------------------------------------
;   Level 03 Data
;-------------------------------------------------------------------------------
Level03Data:
.byte   3, 0, 0, 0, 0, 0, 3     ; row 1
.byte   0, 3, 0, 0, 0, 3, 0
.byte   0, 3, 3, 3, 3, 3, 0
.byte   3, 3, 1, 3, 1, 3, 3
.byte   3, 3, 3, 3, 3, 3, 3
.byte   3, 3, 3, 3, 3, 3, 3
.byte   3, 0, 3, 1, 3, 0, 3
.byte   3, 0, 3, 3, 3, 0, 3
.byte   0, 0, 0, 0, 0, 0, 0
.byte   0, 0, 0, 0, 0, 0, 0
.byte   0, 0, 0, 0, 0, 0, 0
.byte   0, 0, 0, 0, 0, 0, 0
;-------------------------------------------------------------------------------

;-------------------------------------------------------------------------------
;   Level 04 Data
;-------------------------------------------------------------------------------
Level04Data:
.byte   1, 1, 1, 1, 1, 1, 1     ; row 1
.byte   2, 2, 0, 0, 0, 2, 2
.byte   3, 3, 3, 4, 3, 3, 3
.byte   0, 0, 0, 0, 0, 0, 0
.byte   0, 0, 0, 0, 0, 0, 0
.byte   0, 0, 0, 0, 0, 0, 0
.byte   0, 0, 0, 0, 0, 0, 0
.byte   0, 0, 0, 0, 0, 0, 0
.byte   0, 0, 0, 0, 0, 0, 0
.byte   0, 0, 0, 0, 0, 0, 0
.byte   0, 0, 0, 0, 0, 0, 0
.byte   0, 0, 0, 0, 0, 0, 0
;-------------------------------------------------------------------------------

;-------------------------------------------------------------------------------
;   Level 05 Data
;-------------------------------------------------------------------------------
Level05Data:
.byte   1, 1, 1, 1, 1, 1, 1     ; row 1
.byte   2, 2, 0, 0, 0, 2, 2
.byte   3, 3, 3, 4, 3, 3, 3
.byte   0, 0, 0, 0, 0, 0, 0
.byte   0, 0, 0, 0, 0, 0, 0
.byte   0, 0, 0, 0, 0, 0, 0
.byte   0, 0, 0, 0, 0, 0, 0
.byte   0, 0, 0, 0, 0, 0, 0
.byte   0, 0, 0, 0, 0, 0, 0
.byte   0, 0, 0, 0, 0, 0, 0
.byte   0, 0, 0, 0, 0, 0, 0
.byte   0, 0, 0, 0, 0, 0, 0
;-------------------------------------------------------------------------------
