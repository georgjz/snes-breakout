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
;   File: WRAMPointers.inc
;   Author(s): Georg Ziegler
;   Description: This file is a header, include it to access zero page pointers
;

.ifndef WRAMPOINTERS_INC
.define WRAMPOINTERS_INC

;-------------------------------------------------------------------------------
;   Input Pointers
;-------------------------------------------------------------------------------
.import     Joy1Raw             ; Buttons pressed last frame
.import     Joy1Trig            ; Buttons pressed this frame
.import     Joy1Held            ; Buttons held from last frame
.import     Joy2Raw             ; Buttons pressed last frame
.import     Joy2Trig            ; Buttons pressed this frame
.import     Joy2Held            ; Buttons held from last frame
.import     OAMBuffer           ; Buffer for OAM data that is updated every frame
.import     GameState           ; Holds the current game state
.import     LevelToLoad         ; Holds the next level that should be loaded
.import     BallSticky          ; Holds wether the ball is sticked to the paddle
.import     Ball                ; Holds the data of the ball
.import     Paddle              ; Holds the data of the paddle
.import     BG1HOffset          ; Background 1 horizontal offset
.import     BG1VOffset          ; Background 1 vertical offset
.import     BG2HOffset          ; Background 2 horizontal offset
.import     BG2VOffset          ; Background 2 vertical offset
.import     BG3HOffset          ; Background 3 horizontal offset
.import     BG3VOffset          ; Background 3 vertical offset
;-------------------------------------------------------------------------------

.endif  ; WRAMPOINTERS_INC
