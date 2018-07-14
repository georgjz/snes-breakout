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
;   File: GameConstants.inc
;   Author(s): Georg Ziegler
;   Description: This a header file contains constants used throughout the game.
;

.ifndef GAMECONSTANTS_INC
.define GAMECONSTANTS_INC

;-------------------------------------------------------------------------------
;   Game Constants
;-------------------------------------------------------------------------------
PADDLE_START_HPOS   = $6c
PADDLE_START_VPOS   = $d0
UPPER_BOUNDRY       = $11
LOWER_BOUNDRY       = $e0
LEFT_BOUNDRY        = $10
RIGHT_BOUNDRY       = $ef
PADDLE_OAM_OFFSET   = $150
BALL_OAM_OFFSET     = $154
;-------------------------------------------------------------------------------

.endif  ; GACONSTANTS_INC
