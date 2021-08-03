# midi-di

Library for MIDI definitions and for reading MIDI files. It is completely written in D with no dependencies other than
the standard library.

## Simple Example

```d
void main(string[] args) {
    import mididi.reader : readMIDIFile;
    import mididi.writer : writeMIDIFile;
    import std.stdio : stdout;

    if (args.length < 2) {
        stdout.writeln("Too few arguments");
        return;
    }

    // reads a file specified on the command line
    // then writes and reads the file to show that the content is preserved
    const midi = readMIDIFile(args[1]);
    writeMIDIFile("./tmp.midi", midi);
    const copy = readMIDIFile("./tmp.midi");
    assert(midi == copy);
}
```

## Documentation

See [w2ptr.github.io/docs/midi-di](https://w2ptr.github.io/docs/midi-di) for documentation.

## Modules

The `mididi` package contains the following modules:
- `mididi.def`: contains `enum` definitions and some functions that are relevant to MIDI data.
- `mididi.types`: contains the type definitions of the structures used in MIDI.
- `mididi.reader`: contains the parser (`MIDIReader!T`) struct and some convenience functions for reading MIDI files.
- `mididi.writer`: contains the writer (see `writeXYZ` functions) and some convenience functions for writing to MIDI
files.
