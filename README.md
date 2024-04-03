CHIP-8 is an interpreted virtual machine designed for portable video games. It has a much simpler design than most actual hardware and a number of games and opcode extensions created for it. The base design has 35 opcodes with fixed 16-bit big-endian codes.

This project is a Haskell implementation of the CHIP-8 virtual machine for my CS357L class (declarative programming). It's split into 5 files:
* `Chip8.hs` - The main module that contains the virtual machine and its operations.
* `BrickMain.hs` - A simple brick frontend.
* `GlossMain.hs` - A simple gloss frontend.
* `Main.hs` - The main entry point for the program, including the CLI.
* `Util.hs` - A collection of utility functions used by the other modules.

The project description is very open-ended, all we were told was to make "something" using Haskell and either Brick or Gloss. At first, I wanted to emulate SNES, but it was very obvious very quickly it was way too complex for the time I had and my familiarity with Haskell. NES was also too complex, and Atari 2600 was just on the border of complexity. I tried to implement that, but after spending an inordinate amount of time just on decoding, I realized the bus topology makes it very difficult to implement in Haskell without some kind of message-passing system and it was a bit too much. Finally I got to CHIP-8 which I've implemented before, though I lost the code.

Even though we only needed to write a program using Brick or Gloss, I had an interesting idea for how to make it work in a TUI and it just inherently works better as a GUI. Plus, most of the work is in the virtual machine itself.

## Usage
The CLI is a bit brittle because flags are based on pattern matching rather than an actual argparse-esque library, so order matters.

### Controls
The CHIP-8 has a 16-key keypad from the COSMAC which looks like:
```
1 2 3 C
4 5 6 D
7 8 9 E
A 0 B F
```

The emulator maps these keys to:
```
1 2 3 4
Q W E R
A S D F
Z X C V
```

You can also pause/resume the emulator with ENTER, rewind with LEFT, and step through instructions with RIGHT.

### Running
Run an example ROM with all the defaults:
```shell
cabal run
```

Run a specific ROM with brick:
```shell
cabal run exes -- brick path/to/rom
```

Add some breakpoints:
```shell
cabal run exes -- brick path/to/rom -b 200,3BE,40A
```

### Disassembly
Print a disassembly of a ROM:
```shell
cabal run exes -- dis path/to/rom
```

Print the ROM as pseudocode:
```shell
cabal run exes -- pseudo path/to/rom
```

## References
* [CHIP-8 archive](https://johnearnest.github.io/chip8Archive/)
* [CHIP-8 extensions](https://chip-8.github.io/extensions/)
* [Wikipedia](https://en.wikipedia.org/wiki/CHIP-8)
* [Reference implementation](https://github.com/gcsmith/gchip/)