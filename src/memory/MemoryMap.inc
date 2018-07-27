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
;   File: MemoryMap.inc
;   Author(s): Georg Ziegler
;   Description: This file constants to make code that transfers VRAM data more
;   readable
;

.ifndef MEMORYMAP_INC
.define MEMORYMAP_INC

;-------------------------------------------------------------------------------
;   VRAM Map
;-------------------------------------------------------------------------------
SPRITE_DATA_SEG     = $00       ; $0000 in VRAM, address of sprite sheet
    ; 4bpp maps
BORDER_MAP_SEG      = $08       ; $4000 in VRAM, map for displaying the playfield border
OPAQUE_MAP_SEG      = $09       ; $4800 in VRAM, map for a opaque screen
SPLASH_MAP_SEG      = $0a       ; $5000 in VRAM, map for the splash screen
TRANS_MAP_SEG       = $0b       ; $5800 in VRAM, map for a transparent screen
    ; 2bpp maps
START_MENU_SEG      = $0c       ; $6000 in VRAM, map for the start menu
    ; Additional data, remove later
BG1MAP_SEGMENT      = $20       ; $4000 in VRAM, tilemap for background 1
BG2MAP_SEGMENT      = $24       ; $4800 in VRAM, tilemap for background 2
BG3MAP_SEGMENT      = $28       ; $5000 in VRAM, tilemap for background 3
;-------------------------------------------------------------------------------

.endif  ; MEMORYMAP_INC
