# midi-di

Library for MIDI definitions and for reading MIDI files. It is completely written in D with no dependencies other than
the standard library.

## Documentation

There is no documentation page yet, but definitions have in-code documentation comments attached to them. This way, it
should be easy to auto-generate a documentation page from the code.

## Files

The `mididi` package contains the following files:
- `mididi.def`: contains `enum` definitions and some functions that are relevant to MIDI and music/audio.
- `mididi.types`: contains the type definitions of the structures used in MIDI.
- `mididi.reader`: contains the parser (`MIDIReader!T`) struct and some convenience functions for reading MIDI files.
