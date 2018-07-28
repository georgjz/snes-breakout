# Breakout Clone for SNES V0.1
This is a simple homebrew Breakout clone for SNES. It uses my simple [Neko Library](https://github.com/georgjz/neko) for SNES. Both are work in progress.

This clone was inspired by the excellent [clone on learnopengl.com](https://learnopengl.com/In-Practice/2D-Game/Breakout).

This clone calculates the color palettes from a set of base colors found in `src/BaseColors.s`.

## Building
You will need `make` and the [cc65 toolchain](https://github.com/cc65/cc65) to build this project:

```
$ git clone --recursive https://github.com/georgjz/snes-breakout.git
$ cd snes-breakout
$ make
```

The finished ROM should be in the `build/release` subdirectory.

## File Structure
* `gfx/`: contains all the binary graphic data as well as the original aseprite and Tiled files I used to create the graphics
* `levels/`: contains all the level data loaded by the `LoadLevel` subroutine
* `nekolibrary/`: a submodule containing the code from my simple SNES library. Will be "hard-coded" into this repository once it's finished
* `src/`:
    * `gfx/`: all subroutines that handle graphics. Most importantly the `LoadLevel` and `GenerateColors` subroutines
    * `init/`: simple initialization subroutines to set up the SNES
    * `memory/`: files related to the game's memory map
    * `Breakout.s` is the main file, here is where all the magic happens.

## Documentation
I plan to create a more in-depth code documentation once I've implemented all of the basic features I wish to add. For now, please refer to the comments in the code for explanations.

## Missing stuff/To Do
Several features are still missing:
* Once a level is finished, the game will keep running. There is no code to advance to the next level yet.
* This game can only be played in **Ironman Mode**. There are no lives. If you lose, you return to the main screen.
* The brick-ball collision detection logic is a mess. This can lead to some odd/fault behavior now and then.
* The ball has constant speed, the angle between ball and paddle never changes. This will change in the future.
* The weird background is due to a missing feature: as of now I move the destroyed bricks off screen. But in the final version, I wish to hide the destroyed bricks behind a opaque screen (hence the file name in `gfx/`) by simply changing their sprite priority (this is already implementen in `Breakout.s`, actually)

Some parts of the code are very messy. But I follow the old C programming rule here: *First make it run, then make it run fast!*
