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
;   File: LevelLoader.inc
;   Author(s): Georg Ziegler
;   Description: This a header file for LevelLoader.s, include this
;   to use the subroutines in afore mentioned file
;

.ifndef LEVELLOADER_INC
.define LEVELLOADER_INC

;-------------------------------------------------------------------------------
;   Base Color Table Pointer
;-------------------------------------------------------------------------------
.import     LoadLevel           ; Reads a level file and loads data into OAM buffer
;-------------------------------------------------------------------------------

.endif  ; LEVELLOADER_INC
