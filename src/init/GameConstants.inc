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
INITIAL_BALL_HSPEED = $02
INITIAL_BALL_VSPEED = $fe
UPPER_BOUNDRY       = $10
LOWER_BOUNDRY       = $e0
LEFT_BOUNDRY        = $10
RIGHT_BOUNDRY       = $f0
PADDLE_OAM_OFFSET   = $150
BALL_OAM_OFFSET     = $158
HMSB_OAM_OFFSET     = $200
DESTROYED_BRICK     = $1000
DESTROYABLE_BRICK   = $2000
SOLID_BRICK         = $3000
OAM_PRIO_BITS       = $3000
BRICK_HSIZE         = $20
BRICK_VSIZE         = $08
;-------------------------------------------------------------------------------

.endif  ; GACONSTANTS_INC
