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
;   File: GfxData.inc
;   Author(s): Georg Ziegler
;   Description: This file is a header, include it to access graphic data
;

.ifndef GFXDATA_INC
.define GFXDATA_INC

;-------------------------------------------------------------------------------
;   Graphic Addresses
;-------------------------------------------------------------------------------
.import     SpriteSheet:    far
.import     SpritePalette:  far
.import     GameBorderMap:  far
.import     OpaqueMap:      far
.import     SplashMap:      far 
.import     StartMenuMap:   far
;-------------------------------------------------------------------------------

.endif  ; GFXDATA_INC
