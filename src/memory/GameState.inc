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
;   File: GameState.inc
;   Author(s): Georg Ziegler
;   Description: This file game state constants
;

.ifndef GAMESTATE_INC
.define GAMESTATE_INC

;-------------------------------------------------------------------------------
;   Game States
;-------------------------------------------------------------------------------
GAME_STATE_FADE     = $00       ; Game screen is fading in or out
GAME_STATE_MENU     = $01       ; Game is displaying start menu
GAME_STATE_RUN      = $02       ; Game is running
GAME_STATE_LOAD     = $03       ; Game is loading a level
;-------------------------------------------------------------------------------

.endif  ; GAMESTATE_INC
