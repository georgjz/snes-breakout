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
        phd                     ; save caller's D register/frame pointer on stack
        ldx #$00                ; push two empty bytes to stack as local variables
        phx
        ; local symbols for direct addressing (local) variables on stack
        NewHPos = $01
        NewVPos = $02
        tsc                     ; move stack pointer via A...
        tcd                     ; ...to D as subroutine's frame pointer

        ; update paddle
        SetA16
        lda Joy1Trig            ; load buttons last frame...
        ora Joy1Held            ; ...and combine with held buttons
        and # (MASK_BUTTON_LEFT | MASK_BUTTON_RIGHT) ; check if right/left button pressed
        SetA8
        tay                     ; save pressed button in Y
        beq PaddleDone          ; if non pressed, paddle update done
        lda Paddle+ObjData::HPos ; get current horizontal position
        cpy #MASK_BUTTON_LEFT   ; if left button was pressed...
        beq MoveLeft            ; ...move paddle to the left
        clc                     ; else, add horizontal speed to current position
        adc Paddle+ObjData::HSpeed
        sta NewHPos             ; save new position on stack
        lda #RIGHT_BOUNDRY      ; get right playfield boundry
        sec                     ; subtract paddle horizontal size
        sbc Paddle+ObjData::HSize
        cmp NewHPos             ; compare to new position on stack
        bcs UpdatePaddleOAM     ; if new paddle position is smaller than boundry - size, all good
        sta NewHPos             ; else, overwrite new position with boundry - size
        jmp UpdatePaddleOAM     ; and jump to next step
MoveLeft:
        sec                     ; subract paddle horizontal speed
        sbc Paddle+ObjData::HSpeed
        sta NewHPos             ; push new position to stack
        lda #LEFT_BOUNDRY       ; get left playfield boundry
        cmp NewHPos             ; compare to new position
        bcc UpdatePaddleOAM     ; if left boundry smaller than new position, all good
        sta NewHPos             ; else, overwrite new horizontal position with left boundry
UpdatePaddleOAM:
        lda NewHPos             ; pull new horizontal position from stack
        sta Paddle+ObjData::HPos ; store new horizontal position
        ldx #PADDLE_OAM_OFFSET  ; use X as offset into OAM buffer
        sta OAMBuffer, X        ; store new paddle positions on OAM buffer
        clc                     ; add 32/$20 for second sprite
        adc #$20
        sta OAMBuffer + $04, X  ; store positions for second sprite
PaddleDone:

        ;   calculate new position
        ;   check for collision with walls
        ;   if collision
        ;       reset position to touch wall
        ; update paddle OAM

        ; update ball
        ;
        ; calculate new position and save on stack
        lda Ball+ObjData::HPos      ; get current horizontal position
        clc                         ; add horizontal speed
        adc Ball+ObjData::HSpeed
        sta NewHPos                 ; save new horizontal position on stack
        lda Ball+ObjData::VPos      ; get current vertical position
        clc                         ; add vertical speed
        adc Ball+ObjData::VSpeed
        sta NewVPos                 ; save new vertical position on stack
        ; create a frame pointer for direct addressing on stack
        ; tsc                         ; move stack pointer via A...
        ; tcd                         ; ...to D
        ; ; constants used for more verbose code in direct addressing
        ; BallVPos = $01
        ; BallHPos = $02
        ;++++++++++++++++++++++++++++++++++++++++++++++++
;         ; check if ball collides with paddle
;         ; check horizontal collision axis
;         ; check if ball's right edge is to the right of paddle's left edge
;         ; lda BallHPos, S             ; get new position
;         lda BallHPos                ; get new position
;         clc                         ; add horizontal size of ball
;         adc Ball+ObjData::HSize
;         cmp Paddle+ObjData::HPos
;         bcc PaddleCollisionDone     ; if not, no collision
;         ; check if ball's left edge is to the right of paddle's left edge
;         lda Paddle+ObjData::HPos    ; get current horizontal position of paddle
;         clc                         ; add horizontal paddle size
;         adc Paddle+ObjData::HSize
;         ; cmp BallHPos, S             ; compare to the new horizontal position/left edge of ball
;         cmp BallHPos                ; compare to the new horizontal position/left edge of ball
;         bcc PaddleCollisionDone     ; if not, no collision
;         ; check vertical collision axis
;         ; check if ball's lower edge is below paddle's upper edge
;         ; lda BallVPos, S             ; get new vertical position of ball
;         lda BallVPos                ; get new vertical position of ball
;         clc                         ; add verticall ball size
;         adc Ball+ObjData::VSize
;         cmp Paddle+ObjData::VPos
;         bcc PaddleCollisionDone     ; if not, no collision
;         ; check if paddle's lower edge is above ball's upper edge
;         lda Paddle+ObjData::VPos    ; get vertical position of paddle
;         clc                         ; add vertical size of paddle
;         adc Paddle+ObjData::VSize
;         ; cmp BallVPos, S             ; compare to new vertical position of ball
;         cmp BallVPos                ; compare to new vertical position of ball
;         bcc PaddleCollisionDone     ; if not, no collision
;         ; handle collision between ball and paddle
;         ; check wether ball midpoint is above paddle
;         ; lda BallHPos, S             ; get new horizontal ball position
;         lda BallHPos                ; get new horizontal ball position
;         clc                         ; add $04 to get ball midpoint
;         adc #$04
;         sec                         ; subtract horizontal paddle position
;         sbc Paddle+ObjData::HPos
;         cmp #$00
;         bcc PaddleEdgeCollision     ; if ball midpoint is not to right of paddle's left edge, then edge collision occured
;         cmp Paddle+ObjData::HSize   ; if ball midpoint is above or left of paddle'es right edge...
;         bcs PaddleEdgeCollision     ; then edge collision occured
;         ; simple collision with paddle, so reposition ball above paddle and invert vertical speed
;         lda Paddle+ObjData::VPos    ; get vertical position of paddle
;         sec                         ; subtract ball size
;         sbc Ball+ObjData::VSize
;         ; sta BallVPos, S             ; update new vertical ball position
;         sta BallVPos                ; update new vertical ball position
;         lda #$00                    ; reset A to store inverted VSpeed
;         sec                         ; 0 - VSpeed = -VSpeed
;         sbc Ball+ObjData::VSpeed
;         sta Ball+ObjData::VSpeed
;         jmp UpdateBallOAM           ; jump to update the OAM buffer
; PaddleEdgeCollision:
;         ; calculate delta H and delta V
;         ; lda BallHPos, S             ; get the new horizontal position
;         lda BallHPos                ; get the new horizontal position
;         sec                         ; subtract horizontal paddle position
;         sbc Paddle+ObjData::HPos
;         tax                         ; save delta H in X
;         ; lda BallVPos, S             ; get new vertical position
;         lda BallVPos                ; get new vertical position
;         sec                         ; subract vertical paddle position
;         sbc Paddle+ObjData::VPos
;         pha                         ; push delta V to stack
;         txa                         ; move delta H back to A
;         sec                         ; prepare carry for SBC
;         sbc $01, S                  ; delta H - delta V
;         bvc :+                      ; if V clear, then N = N xor 1, else V xor N = N xor 1
;         eor #$8000                  ; calculate N xor V
; :       bpl :+                      ; if delta H >= delta V, skip


UpdateBallOAM:
        SetA16
        lda NewHPos             ; get new position from stack
        sta Ball+ObjData::HPos  ; save new position
        ldx #BALL_OAM_OFFSET    ; get ball of set into OAM buffer
        sta OAMBuffer, X        ; update ball OAM buffer data
        SetA8



PaddleCollisionDone:


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
        ; reset ball OAM
        SetA16
        ldx #BALL_OAM_OFFSET    ; use X as offset into OAM buffer
        lda Ball+ObjData::HPos  ; get current ball position
        sta OAMBuffer, X        ; store ball position in OAM buffer
        lda #$3008              ; no flip, palette 0, name $08
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
        ; PreserveRegisters           ; preserve working registers
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
        inc $01, X                  ; increment screen brightness by one
        lda $01, X                  ; load new screen brightness
        sta INIDISP                 ; set new screen brightness
        cmp #$0f                    ; check if brightness has reached max...
        bcs FadeLoopDone            ; ...then the fade is done
        jmp FadeLoop                ; redo loop
FadeLoopDone:
        pla                         ; increment stack pointer
        pla                         ; pull old game state...
        sta GameState               ; ...and restore it

        ; RestoreRegisters            ; restore working registers
        rts
.endproc
;-------------------------------------------------------------------------------

;-------------------------------------------------------------------------------
;   Fade out screen
;-------------------------------------------------------------------------------
.proc   FadeOut
        ; PreserveRegisters           ; preserve working registers
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
        dec $01, X                  ; decrement screen brightness by one
        lda $01, X                  ; load new screen brightness
        sta INIDISP                 ; set new screen brightness
        cmp #$00                    ; check if brightness has reached zero...
        bcs FadeLoopDone            ; ...then the fade is done
        jmp FadeLoop                ; redo loop
FadeLoopDone:
        pla                         ; increment stack pointer
        pla                         ; pull old game state...
        sta GameState               ; ...and restore it

        ; RestoreRegisters            ; restore working registers
        rts
.endproc
;-------------------------------------------------------------------------------
