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
.include "GameInitialization.inc"
.include "GameState.inc"
.include "ColorGenerator.inc"
.include "LevelLoader.inc"
.include "Levels.inc"
.include "MemoryMap.inc"
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
;   This is the entry point of the cradle
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

        ; check for start button when in menu state
        ; SetA16
        ; lda Joy1Trig            ; load the buttons pressed last frame
        ; and #MASK_BUTTON_START  ; check if start button was pressed
        ; beq GameLoopDone        ; if start button not pressed, game loop done
        ; SetA8
        ; jsr FadeOut             ; fade out screen to begin level loading
        ; lda # (FORCED_BLANKING_ON) ; turn on forced blanking
        ; sta INIDISP
        ; stz NMITIMEN            ; disable NMI
        ; ; load level 01
        ; ;   set BG1 to border
        ; lda # (BORDER_MAP_SEG << 2 | BG1_SC_SIZE_32)
        ; sta BG1SC
        ; tsx                     ; save stack pointer
        ; PushFarAddr OAMBuffer   ; pass pointer to OAM buffer
        ; PushFarAddr Level01Data ; pass pointer to level data
        ; jsl LoadLevel           ; load level
        ; txs                     ; restore stack pointer
        ; PushFarAddr OAMBuffer   ; pass pointer to OAM buffer
        ; lda #UpdateOAMRAMOpcode
        ; jsl NekoLibLauncher
        ; txs                     ; restore stack pointer
        ; ;   set game state to run
        ; lda #GAME_STATE_RUN
        ; sta GameState
        ; ;   turn off forced blanking and turn on NMI
        ; lda # (FORCED_BLANKING_OFF | $0f)
        ; sta INIDISP
        ; lda #$13                ; turn on BG1, BG2, and OBJs
        ; sta TM
        ; lda #$81
        ; sta NMITIMEN            ; turn on MNI
        ; jsr FadeIn              ; fade into new level

        ; if game state = menu
        ;   if start button pressed
        ;       fade out
        ;       turn off NMI, forced blanking on
        ;       load level
        ;       turn on NMI, forced blanking off
        ;       fade in
        ; update background offsets

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
        and # (GAME_STATE_FADE | GAME_STATE_LOAD)
        bne NMIHandlerDone          ; ...skip all calls in NMI

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
        ; code
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

        ; reset paddle and ball

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
