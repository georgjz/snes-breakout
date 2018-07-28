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
;   File: Breakout.s
;   Author(s): Georg Ziegler
;   Description: Entry point for Breakout SNES game
;

;-------------------------------------------------------------------------------
;   Includes
;-------------------------------------------------------------------------------
.include "SNESRegisters.inc"
.include "NekoLib.inc"
.include "GameConstants.inc"
.include "GameInitialization.inc"
.include "GameState.inc"
.include "ColorGenerator.inc"
.include "LevelLoader.inc"
.include "Levels.inc"
.include "MemoryMap.inc"
.include "ObjStruct.inc"
.include "WRAMPointers.inc"
;-------------------------------------------------------------------------------

;-------------------------------------------------------------------------------
;   Assembler Directives
;-------------------------------------------------------------------------------
.p816
.i16
.a8
;-------------------------------------------------------------------------------

;-------------------------------------------------------------------------------
;   Exports of subroutines for use in other files
;-------------------------------------------------------------------------------
.export     ResetHandler
.export     NMIHandler
.export     IRQHandler
;-------------------------------------------------------------------------------

.segment "CODE"
;-------------------------------------------------------------------------------
;   This is the entry point of the game
;-------------------------------------------------------------------------------
.proc   ResetHandler
        SetXY16
        SetA8
        ldx #$1fff              ; set up stack
        txs
        ; force v-blanking, full screen brightness
        lda # ($0f | FORCED_BLANKING_ON)
        sta INIDISP
        stz NMITIMEN            ; disable NMI

        ; clear SNES
        lda #ClearRegistersOpcode ; clear all registers to standard values
        jsl NekoLibLauncher
        lda #ClearVRAMOpcode    ; clear VRAM to zero/$00
        jsl NekoLibLauncher
        lda #ClearCGRAMOpcode   ; clear CG-RAM to zero/$00
        jsl NekoLibLauncher
        phk                     ; restore data bank register
        plb

        ; init game data
        jsl InitGame
        jsl ResetOAMBuffer
        jsl GenerateColors
        jsl InitVariables
        phk                     ; restore data bank register
        plb

        ; intro
        lda #GAME_STATE_FADE    ; set game state to fading
        sta GameState
        lda #$01                ; make BG1 visible
        sta TM
        lda # (FORCED_BLANKING_OFF | $00) ; turn off forced blanking, screen brightness to zero
        sta INIDISP
        lda #$81                ; enable NMI
        sta NMITIMEN

        ; intro fade
        jsr FadeIn

        ; intro scroll
ScrollLoop:
        wai                     ; wait for NMI
        SetA16
        lda BG1VOffset          ; get current offset
        dec                     ; decrement offset by one
        sta BG1VOffset          ; store new offset
        SetA8
        sta BG1VOFS             ; store new offset, low byte
        xba                     ; clear upper 3 bits...
        and #$1f                ; of high byte of new offset
        sta BG1VOFS             ; store new offset, bits 8 ~ 12
        xba                     ; restore new offset, sets Z flag
        beq ScrollLoopDone      ; if offset is zero, scrolling finished
        jmp ScrollLoop
ScrollLoopDone:
        tax                     ; transfer to X for 16-bit operations
        stx BG1VOffset          ; save new offset
        lda #GAME_STATE_MENU    ; change game state to menu
        sta GameState

        nop                     ; break point for debugger

        jml GameLoop
.endproc
;-------------------------------------------------------------------------------

;-------------------------------------------------------------------------------
;   After the ResetHandler will jump to here
;-------------------------------------------------------------------------------
.proc   GameLoop
        SetA8                   ; safe guard against weird RTI behavior
        wai                     ; wait for NMI / V-Blank

        ; Call GameLoopLauncher
        lda GameState           ; get the current game state
        jsr GameLoopLauncher    ; call the appropiate handler

        ; if game state = run
        ;   update Paddle
        ;   update Ball
        ;       do collision checks

GameLoopDone:
        jmp GameLoop
.endproc
;-------------------------------------------------------------------------------

;-------------------------------------------------------------------------------
;   Will be called during V-Blank
;-------------------------------------------------------------------------------
.proc   NMIHandler
        .a8
        lda RDNMI                   ; read NMI status, acknowledge NMI

        ; check game state
        lda GameState               ; get current game state
        ; if game is fading or loading...
        and # (GAME_STATE_LOAD)
        beq NMIHandlerDone          ; ...skip all calls in NMI

        ; read input
        tsx                         ; save stack pointer
        PushFarAddr Joy1Raw         ; push address of where to store joypad data
        lda #PollJoypad1Opcode
        jsl NekoLibLauncher         ; poll joypad data and store in provided pointer

        ; transfer OAM data
        txs                         ; restore stack pointer
        PushFarAddr OAMBuffer       ; push source address to stack
        lda #UpdateOAMRAMOpcode
        jsl NekoLibLauncher
        txs                         ; restore stack pointer

NMIHandlerDone:
        rti                         ; NMI interrupt done
.endproc
;-------------------------------------------------------------------------------

;-------------------------------------------------------------------------------
;   This is not used in this program
;-------------------------------------------------------------------------------
.proc   IRQHandler
        ; code
        rti
.endproc
;-------------------------------------------------------------------------------

; The current game state is also the opcode for the handling subroutine. that
; subroutine will handle all operations required in that state

;-------------------------------------------------------------------------------
;   Called in GameLoop to handle all steps
;-------------------------------------------------------------------------------
.proc   GameLoopLauncher
        ldx #$00                    ; clear X
        xba                         ; make sure B is clear/zero
        and #$00
        xba
        tax                         ; transfer opcode to X
        phk                         ; switch data bank register...
        plb                         ; ...to current program bank
        lda GameLoopRTSTableH, X    ; get high byte of subroutine address...
        pha                         ; ...and push to stack
        lda GameLoopRTSTableL, X    ; get low byte of subroutine address...
        pha                         ; ...and push to stack
        rts                         ; call subroutine stored on stack
.endproc
;-------------------------------------------------------------------------------

;----- Opcode/RTS Table --------------------------------------------------------
GameLoopRTSTableL:
.byte   <(HandleFadeState - 1)
.byte   <(HandleMenuState - 1)
.byte   <(HandleRunState  - 1)
.byte   <(HandleLoadState - 1)

GameLoopRTSTableH:
.byte   >(HandleFadeState - 1)
.byte   >(HandleMenuState - 1)
.byte   >(HandleRunState  - 1)
.byte   >(HandleLoadState - 1)
;-------------------------------------------------------------------------------

;-------------------------------------------------------------------------------
;   Subroutine: HandleFadeState
;   Parameters: -
;   Description: Fades the screen in or out
;-------------------------------------------------------------------------------
.proc   HandleFadeState
        ; code
        rts
.endproc
;-------------------------------------------------------------------------------

;-------------------------------------------------------------------------------
;   Subroutine: HandleMenuState
;   Parameters: -
;   Description: Checks the input while the menu is displayed
;-------------------------------------------------------------------------------
.proc   HandleMenuState
        ; check if start button was pressed
        SetA16
        lda Joy1Trig            ; get buttons pressed last frame
        and #MASK_BUTTON_START  ; check if start button was pressed...
        beq HandleMenuStateDone ; ...if not, then handler is done
        ; else, change state to load level
        SetA8
        lda #$00                ; set level to load to 0
        sta LevelToLoad
        lda #GAME_STATE_LOAD    ; set game state to load
        sta GameState

HandleMenuStateDone:
        SetA8
        rts
.endproc
;-------------------------------------------------------------------------------

;-------------------------------------------------------------------------------
;   Subroutine: HandleRunState
;   Parameters: -
;   Description: Handles all the game logic while the game is running
;-------------------------------------------------------------------------------
.proc   HandleRunState
        phd                         ; save caller's D register/frame pointer on stack
        ldx #$00                    ; push two empty bytes to stack as local variables
        phx
        ; local symbols for direct addressing (local) variables on stack
        NewHPos = $01
        NewVPos = $02
        tsc                         ; move stack pointer via A...
        tcd                         ; ...to D as subroutine's frame pointer

        ; update paddle
        SetA16
        lda Joy1Trig                ; load buttons last frame...
        ora Joy1Held                ; ...and combine with held buttons
        and # (MASK_BUTTON_LEFT | MASK_BUTTON_RIGHT) ; check if right/left button pressed
        SetA8
        tay                         ; save pressed button in Y
        beq PaddleDone              ; if non pressed, paddle update done
        lda Paddle+ObjData::HPos    ; get current horizontal position
        cpy #MASK_BUTTON_LEFT       ; if left button was pressed...
        beq MoveLeft                ; ...move paddle to the left
        clc                         ; else, add horizontal speed to current position
        adc Paddle+ObjData::HSpeed
        sta NewHPos                 ; save new position on stack
        lda #RIGHT_BOUNDRY          ; get right playfield boundry
        sec                         ; subtract paddle horizontal size
        sbc Paddle+ObjData::HSize
        cmp NewHPos                 ; compare to new position on stack
        bcs UpdatePaddleOAM         ; if new paddle position is smaller than boundry - size, all good
        sta NewHPos                 ; else, overwrite new position with boundry - size
        jmp UpdatePaddleOAM         ; and jump to next step
MoveLeft:
        sec                         ; subract paddle horizontal speed
        sbc Paddle+ObjData::HSpeed
        sta NewHPos                 ; push new position to stack
        lda #LEFT_BOUNDRY           ; get left playfield boundry
        cmp NewHPos                 ; compare to new position
        bcc UpdatePaddleOAM         ; if left boundry smaller than new position, all good
        sta NewHPos                 ; else, overwrite new horizontal position with left boundry
UpdatePaddleOAM:
        lda NewHPos                 ; pull new horizontal position from stack
        sta Paddle+ObjData::HPos    ; store new horizontal position
        ldx #PADDLE_OAM_OFFSET      ; use X as offset into OAM buffer
        sta OAMBuffer, X            ; store new paddle positions on OAM buffer
        clc                         ; add 32/$20 for second sprite
        adc #$20
        sta OAMBuffer + $04, X      ; store positions for second sprite
PaddleDone:

        ; check if A button was pressed, then unstick ball from paddle
        ; TODO: replace lda/sta of BallSticky with tsb/tsb
        SetA16
        lda Joy1Trig                ; load buttons pressed last frame...
        ora Joy1Held                ; ...and combine with buttons held
        and #MASK_BUTTON_A          ; check if A button was pressed/held
        SetA8
        tay                         ; check whether A is zero
        beq :+                      ; if so, skip
        lda #$00                    ; else, unstick ball from paddle
        sta BallSticky
:
        ; if ball is sticky, update horizontal ball position
        lda BallSticky
        beq UpdateBall              ; if ball is not sticky, skip
        lda Paddle+ObjData::HPos    ; get updated horizontal paddle position
        clc                         ; add $10 to it
        adc #$10
        sta NewHPos                 ; store new horizontal ball position
        lda Paddle+ObjData::VPos    ; get vertical paddle position
        sec                         ; subtract ball vertical size
        sbc Ball+ObjData::VSize
        sta NewVPos                 ; store result as new vertical ball position
        jmp UpdateBallOAM           ; skip collision detection and update OAM buffer

        ;   calculate new position
        ;   check for collision with walls
        ;   if collision
        ;       reset position to touch wall
        ; update paddle OAM

        ; update ball
UpdateBall:
        ; calculate new position and save on stack
        lda Ball+ObjData::HPos      ; get current horizontal position
        clc                         ; add horizontal speed
        adc Ball+ObjData::HSpeed
        sta NewHPos                 ; save new horizontal position on stack
        lda Ball+ObjData::VPos      ; get current vertical position
        clc                         ; add vertical speed
        adc Ball+ObjData::VSpeed
        sta NewVPos                 ; save new vertical position on stack

        ; check ball-wall collision
        ; right boundry
        lda #RIGHT_BOUNDRY          ; get right playfield boundry
        sec                         ; subtract horizontal ball size
        sbc Ball+ObjData::HSize
        cmp NewHPos                 ; compare to updated horizontal position
        bcs :+                      ; if ball is to left of right boundry - size, skip to left boundry
        sta NewHPos                 ; else, reposition ball
        lda #$00                    ; and invert horizontal speed by...
        sec                         ; ...subtracting speed from zero
        sbc Ball+ObjData::HSpeed
        sta Ball+ObjData::HSpeed    ; store inverted speed
        jmp UpdateBallOAM
        ; left boundry
:       lda #LEFT_BOUNDRY           ; get left playfield boundry
        cmp NewHPos                 ; compare to new position
        bcc :+                      ; if ball is to right of left boundry, skip to upper boundry
        sta NewHPos                 ; else, reposition ball
        lda #$00                    ; and invert horizontal speed by...
        sec                         ; ...subtracting speed from zero
        sbc Ball+ObjData::HSpeed
        sta Ball+ObjData::HSpeed    ; store inverted speed
        jmp UpdateBallOAM
        ; upper boundry
:       lda #UPPER_BOUNDRY          ; get upper playfield boundry
        cmp NewVPos                 ; compare to updated vertical ball position
        bcc :+                      ; if ball is below upper boundry, skip to lower boundry
        sta NewVPos                 ; else, reposition ball
        lda #$00                    ; and invert ball speed...
        sec                         ; ...by subtracting speed from zero
        sbc Ball+ObjData::VSpeed
        sta Ball+ObjData::VSpeed    ; store inverted speed
        ; lower boundry, game over
:       lda #LOWER_BOUNDRY          ; get lower playfield boundry
        cmp NewVPos                 ; compare to updated vertical ball position
        bcs :+                      ; if ball is above lower boundry, skip
        ; else, game over, fade out and return to start menu
        jsr FadeOut                 ; fade out
        lda # (FORCED_BLANKING_ON)  ; turn on forced blanking
        sta INIDISP
        stz NMITIMEN                ; turn off NMI
        ; set background 1 to splash screen
        lda # (SPLASH_MAP_SEG << 2 | BG1_SC_SIZE_32)
        sta BG1SC
        lda #$01                    ; make BG1 visible
        sta TM
        stz LevelToLoad             ; reset level to load to zero
        ; turn on forced blanking and NMI
        lda # (FORCED_BLANKING_OFF | $00)
        sta INIDISP
        lda #$81
        sta NMITIMEN
        jsr FadeIn                  ; fade in
        lda #GAME_STATE_MENU        ; set game state to menu
        sta GameState
        jmp Done                    ; exit subroutine
:

        ; check if ball collides with paddle
        ; check horizontal collision axis
        ; check if ball's right edge is to the right of paddle's left edge
        lda NewHPos                 ; get new position
        clc                         ; add horizontal size of ball
        adc Ball+ObjData::HSize
        cmp Paddle+ObjData::HPos
        ; bcc PaddleCollisionDone     ; if not, no collision
        bcs :+
        jmp PaddleCollisionDone
        ; check if ball's left edge is to the right of paddle's left edge
:       lda Paddle+ObjData::HPos    ; get current horizontal position of paddle
        clc                         ; add horizontal paddle size
        adc Paddle+ObjData::HSize
        cmp NewHPos                 ; compare to the new horizontal position/left edge of ball
        ; bcc PaddleCollisionDone      ; if not, no collision
        bcs :+
        jmp PaddleCollisionDone
        ; check vertical collision axis
        ; check if ball's lower edge is below paddle's upper edge
:       lda NewVPos                 ; get new vertical position of ball
        clc                         ; add verticall ball size
        adc Ball+ObjData::VSize
        cmp Paddle+ObjData::VPos
        ; bcc PaddleCollisionDone     ; if not, no collision
        bcs :+
        jmp PaddleCollisionDone
        ; check if paddle's lower edge is above ball's upper edge
:       lda Paddle+ObjData::VPos    ; get vertical position of paddle
        clc                         ; add vertical size of paddle
        adc Paddle+ObjData::VSize
        cmp NewVPos                 ; compare to new vertical position of ball
        ; bcc PaddleCollisionDone     ; if not, no collision
        bcs :+
        jmp PaddleCollisionDone
:
        ; .byte 42, 00
        ; handle collision between ball and paddle
        ; check whether ball midpoint is above paddle
        ; TODO: Remove static data
        lda NewHPos                 ; get new position
        sec
        sbc Paddle+ObjData::HPos    ; compare to horizontal paddle position
        dec                         ; if result is < 0...
        bmi PaddleLeftEdgeCollision ; ...then go for edge collision
        sec                         ; check right margin
        sbc Paddle+ObjData::HSize
        clc
        adc #$06
        bpl PaddleRightEdgeCollision
        lda Paddle+ObjData::VPos    ; reposition ball on vertical axis
        sec
        sbc Ball+ObjData::VSize
        sta NewVPos
        lda #$00                    ; invert vertical speed
        sec
        sbc Ball+ObjData::VSpeed
        sta Ball+ObjData::VSpeed
        jmp UpdateBallOAM

        ; check for paddle edge collision
PaddleLeftEdgeCollision:
        .byte $42, $00              ; breakpoint
        lda NewVPos
        clc
        adc #$04
        cmp Paddle+ObjData::VPos
        bcc :+
        lda Paddle+ObjData::HPos    ; reposition horizontally
        sec
        sbc Ball+ObjData::HSize
        sta NewHPos
        lda #$00                    ; invert horizontal speed
        sec
        sbc Ball+ObjData::HSpeed
        sta Ball+ObjData::HSpeed
        jmp UpdateBallOAM
:       lda Paddle+ObjData::VPos    ; reposition vertically
        sec
        sbc Ball+ObjData::VSize
        sta NewVPos
        lda #$00                    ; invert vertical speed
        sec
        sbc Ball+ObjData::VSpeed
        sta Ball+ObjData::VSpeed
        lda Ball+ObjData::HSpeed
        bmi :+                      ; if h speed is negative, skip
        lda #$00                    ; else, invert horizontal speed
        sec
        sbc Ball+ObjData::HSpeed
        sta Ball+ObjData::HSpeed
:       jmp UpdateBallOAM
        ; if (Ball.VPos + 4 >= Paddle.VPos)
        ;   NewHPos = Paddle.HPos - Ball.HSize
        ;   flip H speed
        ;
        ; else
        ;   NewVPos = Paddle.VPos - Ball.VSize
        ;   flip V speed
        ;   if (H speed > 0)
        ;       flip H speed
        ;
PaddleRightEdgeCollision:
        .byte $42, $00              ; breakpoint
        lda NewVPos
        clc
        adc #$04
        cmp Paddle+ObjData::VPos
        bcc :+
        lda Paddle+ObjData::HPos    ; reposition horizontally
        clc
        adc Paddle+ObjData::HSize
        sta NewHPos
        lda #$00                    ; invert horizontal speed
        sec
        sbc Ball+ObjData::HSpeed
        sta Ball+ObjData::HSpeed
        jmp UpdateBallOAM
:       lda Paddle+ObjData::VPos    ; reposition vertically
        sec
        sbc Ball+ObjData::VSize
        sta NewVPos
        lda #$00                    ; invert vertical speed
        sec
        sbc Ball+ObjData::VSpeed
        sta Ball+ObjData::VSpeed
        lda Ball+ObjData::HSpeed
        bpl :+                      ; if h speed is negative, skip
        lda #$00                    ; else, invert horizontal speed
        sec
        sbc Ball+ObjData::HSpeed
        sta Ball+ObjData::HSpeed
:       jmp UpdateBallOAM
        ; if (Ball.VPos + 4 >= Paddle.VPos)
        ;   NewHPos = Paddle.HPos + Paddle.HSize
        ;   flip H speed
        ;
        ; else
        ;   NewVPos = Paddle.VPos - Ball.VSize
        ;   flip V speed
        ;   if (H speed < 0)
        ;       flip H speed


PaddleCollisionDone:
        ; .byte $42, $00          ; breakpoint
        ; check ball-brick collisions
        ; A - calculated data
        ; X - auxiliar
        ; Y - brick counter
        ldy #$00
BrickLoop:
        SetA16
        lda OAMBuffer+2, Y      ; get brick OAM attribute bytes
        and #OAM_PRIO_BITS      ; mask prio bits
        eor #DESTROYED_BRICK    ; check if brick is already destroyed...
        beq BrickDone           ; ...if so, go to next brick
        ; else, conduct AABB check between ball and brick
        tax                     ; save vertical and horizontal position in X
        SetA8                   ; brick horizontal position now in A, vertical in B
        ; check if ball's right edge is to the right of brick's left edge
        lda NewHPos                 ; get new position
        clc                         ; add horizontal size of ball
        adc Ball+ObjData::HSize
        cmp OAMBuffer, Y            ; compare to brick's horizontal position
        bcs :+
        jmp BrickDone
        ; check if ball's left edge is to the right of brick's left edge
:       lda OAMBuffer, Y            ; get horizontal position of brick
        clc                         ; add horizontal brick size
        adc #BRICK_HSIZE
        cmp NewHPos                 ; compare to the new horizontal position/left edge of ball
        bcs :+
        jmp BrickDone
        ; check vertical collision axis
        ; check if ball's lower edge is below brick's upper edge
:       lda NewVPos                 ; get new vertical position of ball
        clc                         ; add verticall ball size
        adc Ball+ObjData::VSize
        cmp OAMBuffer+1, Y
        bcs :+
        jmp BrickDone
        ; check if bricks's lower edge is above ball's upper edge
:       lda OAMBuffer+1, Y          ; get vertical position of brick
        clc                         ; add vertical size of brick
        adc #BRICK_VSIZE
        cmp NewVPos                 ; compare to new vertical position of ball
        bcs :+
        jmp BrickDone
:

        ; get direction

        ; reposition ball

        ; flip speed(s)

        .byte $42, $00
        ; check prio
        SetA16
        lda OAMBuffer+2, Y          ; get OAM attribute bytes
        and #OAM_PRIO_BITS          ; mask prio bits
        eor #DESTROYABLE_BRICK      ; if it's not a destroyable brick...
        bne :+                      ; ...skip
        lda OAMBuffer+2, Y
        eor #OAM_PRIO_BITS          ; invert OAM prio bits
        sta OAMBuffer+2, Y
        ; move destroyed brick offscreen by setting HMSB bit
        ; tya                         ; set brick number into A
        ; ShiftARight $02             ; divide by 4
        ; phy                         ; save brick number on stack
        ; ShiftARight $03             ; HMSB table offset in X = Y / 8

:       jmp UpdateBallOAM           ; ball-brick collision done

        ; lda NewVPos
        ; xba
        ; lda NewHPos
        ; check right edge of brick is to the right of ball's left edge
        ; clc
        ; adc Ball+ObjData::HSize
        ; cmp Ball+ObjData::HPos
        ; bcs :+
        ; jmp BrickDone
        ; ;



        ; lda OAMBuffer, Y        ; get brick coordinates
BrickDone:
        iny                     ; increase brick counter by 4
        iny
        iny
        iny
        cpy #$150               ; if Y >= 84 * 4 = 336
        bcc BrickLoop



UpdateBallOAM:
        SetA16
        lda NewHPos             ; get new position from stack
        sta Ball+ObjData::HPos  ; save new position
        ldx #BALL_OAM_OFFSET    ; get ball of set into OAM buffer
        sta OAMBuffer, X        ; update ball OAM buffer data
        SetA8


        ;   do collision checks
        ;       check collision with paddle
        ;           if collision
        ;               calculate new ball speed
        ;               skip brick collision
        ;       check collision with bricks:
        ;       for i : bricks
        ;           if (!brick is visible)
        ;               next
        ;           if (brick and ball collide)
        ;               calculate new ball speed
        ;               if (!brick is solid)
        ;                   set brick visibility to false
        ;               skip remaining bricks
        ;
        ; if level won
        ;   display level
        ;   increase level
        ;   set game state to load
Done:
        plx                     ; kill local variables
        pld                     ; restore caller's D register/frame pointer
        rts
.endproc
;-------------------------------------------------------------------------------

;-------------------------------------------------------------------------------
;   Subroutine: HandleLoadState
;   Parameters: -
;   Description: Loads a new level while the screen is turned off
;-------------------------------------------------------------------------------
.proc   HandleLoadState
        jsr FadeOut             ; fade out
        ; force blanking and turn off NMI
        lda # (FORCED_BLANKING_ON)
        sta INIDISP
        stz NMITIMEN
        ; load level
        lda #$00                ; reset high byte of A
        xba
        PushFarAddr OAMBuffer   ; pass pointer to OAM buffer
        ; TODO: Too complicated, simplify pointer calculation
        lda LevelToLoad         ; load level to load
        clc                     ; "multiply" by 3
        adc LevelToLoad
        clc
        adc LevelToLoad
        tax                     ; transfer level number to X as offset
        lda LevelTable + 2, X   ; get bank of pointer
        pha                     ; push pointer bank to stack
        SetA16
        lda LevelTable, X       ; get high and low byte of pointer
        pha                     ; push pointer address to stack
        SetA8
        jsl LoadLevel           ; load the new level
        plx                     ; restore stack pointer
        plx
        plx

        ; set BG1 to game border
        lda # (BORDER_MAP_SEG << 2 | BG1_SC_SIZE_32)
        sta BG1SC

        ; reset paddle
        lda #PADDLE_START_HPOS  ; set horizontal position
        sta Paddle+ObjData::HPos
        lda #PADDLE_START_VPOS  ; set vertical position
        sta Paddle+ObjData::VPos
        ; rest paddle OAM
        SetA16
        ldx #PADDLE_OAM_OFFSET  ; use X as offset into OAM buffer
        lda Paddle+ObjData::HPos ; get current position data
        tay                     ; save position in Y
        sta OAMBuffer, X
        lda #$3e40              ; no flip, palette 7, name $40
        sta OAMBuffer + $02, X
        tya                     ; restore position in A
        SetA8
        clc                     ; add 32/$20 to horizontal position for second sprite
        adc #$20
        SetA16
        sta OAMBuffer + $04, X  ; set position for second sprite
        lda #$3e44              ; no flip, palette 7, name $44
        sta OAMBuffer + $06, X
        SetA8

        ; reset ball
        lda # (PADDLE_START_HPOS + $10) ; inital horizontal ball position
        sta Ball+ObjData::HPos
        lda # (PADDLE_START_VPOS - $08) ; inital vertical ball position
        sta Ball+ObjData::VPos
        lda #INITIAL_BALL_HSPEED ; set horizontal speed to 2
        sta Ball+ObjData::HSpeed
        lda #INITIAL_BALL_VSPEED ; set vertical speed to -2
        sta Ball+ObjData::VSpeed
        lda #$01                ; set ball to stick to paddle
        sta BallSticky
        ; reset ball OAM
        SetA16
        ldx #BALL_OAM_OFFSET    ; use X as offset into OAM buffer
        lda Ball+ObjData::HPos  ; get current ball position
        sta OAMBuffer, X        ; store ball position in OAM buffer
        lda #$3008              ; no flip, prio 3, palette 0, name $08
        sta OAMBuffer + $02, X
        SetA8

        ; update OAMRAM
        tsx                     ; save stack pointer
        PushFarAddr OAMBuffer   ; pass pointer to OAM buffer
        lda #UpdateOAMRAMOpcode
        jsl NekoLibLauncher
        txs                     ; restore stack pointer

        ; release forced blanking and turn on NMI
        lda # (FORCED_BLANKING_OFF | $0f)
        sta INIDISP
        lda #$13                ; turn on BG1, BG2, and OBJs
        sta TM
        lda #$81                ; enable NMI
        sta NMITIMEN

        ; fade in
        jsr FadeIn

        ; change game state to run
        lda #GAME_STATE_RUN
        sta GameState

        rts
.endproc
;-------------------------------------------------------------------------------

;----- Some helper subroutines -------------------------------------------------

;-------------------------------------------------------------------------------
;   Fade in screen
;-------------------------------------------------------------------------------
.proc   FadeIn
        ; prepare registers for fading
        SetA8
        lda GameState               ; save current game state...
        pha                         ; ...on stack
        lda #GAME_STATE_FADE        ; set game state...
        sta GameState               ; ...to fading
        lda #00                     ; set inital screen brightness...
        sta INIDISP                 ; ...to zero...
        pha                         ; ...and store on stack
        tsx                         ; move stack pointer to X to use as offset
FadeLoop:
        wai                         ; wait for NMI
        inc a:$01, X                ; increment screen brightness by one
        lda a:$01, X                ; load new screen brightness
        sta INIDISP                 ; set new screen brightness
        cmp #$0f                    ; check if brightness has reached max...
        bcs FadeLoopDone            ; ...then the fade is done
        jmp FadeLoop                ; redo loop
FadeLoopDone:
        pla                         ; increment stack pointer
        pla                         ; pull old game state...
        sta GameState               ; ...and restore it

        rts
.endproc
;-------------------------------------------------------------------------------

;-------------------------------------------------------------------------------
;   Fade out screen
;-------------------------------------------------------------------------------
.proc   FadeOut
        ; prepare registers for fading
        SetA8
        lda GameState               ; save current game state...
        pha                         ; ...on stack
        lda #GAME_STATE_FADE        ; set game state...
        sta GameState               ; ...to fading
        lda #$0f                    ; set inital screen brightness...
        sta INIDISP                 ; ...to max...
        pha                         ; ...and store on stack
        tsx                         ; move stack pointer to X to use as offset
FadeLoop:
        wai                         ; wait for NMI
        dec a:$01, X                ; decrement screen brightness by one
        lda a:$01, X                ; load new screen brightness
        sta INIDISP                 ; set new screen brightness
        cmp #$00                    ; check if brightness has reached zero...
        bcs FadeLoopDone            ; ...then the fade is done
        jmp FadeLoop                ; redo loop
FadeLoopDone:
        pla                         ; increment stack pointer
        pla                         ; pull old game state...
        sta GameState               ; ...and restore it

        rts
.endproc
;-------------------------------------------------------------------------------
