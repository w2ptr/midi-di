/**
`mididi.writer` contains functions for encoding MIDI objects to raw bytes.

Currently, the writer is compatible with the standard MIDI format 1.0 and
forward compatible with newer formats (in which case newer features will be
ignored, as the specification demands).

The implementation (and some of the documentation) is based on this specification:
https://www.cs.cmu.edu/~music/cmsip/readings/Standard-MIDI-file-format-updated.pdf

Note:
    these functions do not always check if the provided data (i.e. the chunks
    and track events) is actually correct, but if the data comes from
    `MIDIReader` functions, it should always be complete and valid data

Authors:
    Wout Huynen
*/
module mididi.writer;

// Copyright Wout Huynen 2021.
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE.txt or copy at
// https://www.boost.org/LICENSE_1_0.txt)

import mididi.types;
import std.range.primitives : isOutputRange;
import std.stdio : File;

/**
A utility output range that outputs bytes to a delegate.
*/
struct DelegateSink {
    ///
    this(void delegate(scope const(ubyte)[]) sink) {
        _sink = sink;
    }

    ///
    void put(ubyte x) {
        ubyte[1] bytes = [x];
        _sink(bytes[]);
    }
    ///
    void putMultiple(const ubyte[] bytes) {
        _sink(bytes);
    }

private:
    void delegate(scope const(ubyte)[]) _sink;
}

// the writeMIDIFile overloads are templates so all write* functions are not
// instantiated if it's never even used
/**
`writeMIDIFile` essentially does the same as `writeMIDI` to a range object that
writes to a `std.stdio.File`, but it is given for convenience.

There are two overloads, one for a `std.stdio.File` object and one for a path
(which opens the file for you in write mode).
*/
void writeMIDIFile()(File file, ref const MIDI midi) {
    static struct FileOutputRange {
        void put(ubyte x) {
            ubyte[1] bytes = [x];
            writer.put(bytes[]);
        }
        void putMultiple(scope const ubyte[] bytes) {
            writer.put(bytes);
        }

        typeof(file.lockingBinaryWriter()) writer;
    }
    auto output = FileOutputRange(file.lockingBinaryWriter());
    output.writeMIDI(midi);
}
/// ditto
void writeMIDIFile()(string path, ref const MIDI midi) {
    auto file = File(path, "w");
    writeMIDIFile(file, midi);
    file.close();
}

/**
`writeMIDI` encodes a MIDI data object (`mididi.types.MIDI`) to binary data.

The template parameter `T` must be an output range type defining the
`put(ubyte)` method. In addition, it can define the
`putMultiple(scope const ubyte[])` method if there is a more efficient method
to put several bytes at once. (This info applies to all `write*` functions in
this module.)

Params:
    T = the output range type
    midi = the MIDI object that is encoded into `output`
    output = the output range object
*/
void writeMIDI(T)(ref T output, ref const MIDI midi)
if (isOutputRange!(T, ubyte)) {
    output.writeHeaderChunk(midi.headerChunk);
    assert(midi.headerChunk.nTracks == midi.trackChunks.length,
        "the header's nTracks must be equal to the number of track chunks");
    foreach (ref track; midi.trackChunks) {
        output.writeTrackChunk(track);
    }
}

///
unittest {
    import mididi.def : MetaEventType, SystemMessageType, TrackFormat;

    const(ubyte)[] result;
    auto sink = DelegateSink((scope const bytes) {
        result ~= bytes;
    });
    auto midi = MIDI(
        HeaderChunk(TrackFormat.single, 1, TimeDivision.fromFormat0(1000)),
        [
            TrackChunk([
                TrackEvent(
                    0xFF,
                    MIDIEvent(cast(ubyte) SystemMessageType.songSelect, [123, 0]),
                ),
                TrackEvent(
                    0x0F,
                    MetaEvent(MetaEventType.endOfTrack, []),
                ),
            ]),
        ]
    );
    sink.writeMIDI(midi);
    assert(result == cast(const(ubyte)[]) [
        'M', 'T', 'h', 'd',
        0, 0, 0, 6,
        0, 0,
        0, 1,
        0x03, 0xE8,

        'M', 'T', 'r', 'k',
        0, 0, 0, 8,
        0x81, 0x7F, cast(ubyte) SystemMessageType.songSelect, 123,
        0x0F,       0xFF, cast(ubyte) MetaEventType.endOfTrack, 0x00,
    ]);
}

/**
*/
void writeHeaderChunk(T)(ref T output, ref const HeaderChunk chunk)
if (isOutputRange!(T, ubyte)) {
    output.write(cast(const(ubyte)[]) "MThd");
    output.writeInt32(6); // length
    output.writeInt16(cast(ushort) chunk.trackFormat);
    output.writeInt16(chunk.nTracks);
    output.writeInt16(chunk.division.rawValue);
}

/// This test demonstrates header chunk writing.
unittest {
    import mididi.def : TrackFormat;

    auto chunk = HeaderChunk(
        TrackFormat.sequential,
        ushort.max,
        TimeDivision.fromFormat1(-25, 64),
    );
    auto result = "";
    auto range = DelegateSink((scope const bytes) {
        result ~= bytes;
    });
    range.writeHeaderChunk(chunk);
    assert(result == [
        'M', 'T', 'h', 'd', // chunk: header
        0, 0, 0, 6, // length: 6
        0, 2, // format: 2
        0xFF, 0xFF, // nTracks: ushort.max
        0b1_1100111, 64, // division: format 1; -25; 64
    ]);
}

/**
`writeTrackChunk` encodes a track chunk to bytes and `put`s them into `output`.

This is guaranteed to use "running status" to encode consecutive track events
with the same status byte.

Params:
    output = the output range object
    chunk = the track chunk to be encoded
*/
void writeTrackChunk(T)(ref T output, ref const TrackChunk chunk)
if (isOutputRange!(T, ubyte)) {
    output.write(cast(const(ubyte)[]) "MTrk");

    // unfortunately, we have to write everything to a buffer first so the
    // length can be written before everything else
    // TODO: this can be a stack buffer so it uses less heap memory
    const(ubyte)[] buffer = [];
    auto bufferWriter = DelegateSink((scope const bytes) {
        buffer ~= bytes;
    });
    ubyte runningStatus = 0;
    foreach (ref event; chunk.events) {
        bufferWriter.writeTrackEvent(event, runningStatus);
        runningStatus = event.statusByte;
    }
    output.writeInt32(cast(uint) buffer.length);
    output.write(buffer);
}

/// This test demonstrates track chunk writing
unittest {
    import mididi.def : MetaEventType;

    const(ubyte)[] result = [];
    auto range = DelegateSink((scope const bytes) {
        result ~= bytes;
    });
    auto chunk = TrackChunk([
        TrackEvent(
            100, // delta time
            // 0x93 = note on
            MIDIEvent(0x93, [0x4C, 0x20]),
        ),
        TrackEvent(
            300,
            // 0x93 = note on (same status)
            MIDIEvent(0x93, [0x4C, 0x00]),
        ),
        TrackEvent(
            400,
            MetaEvent(MetaEventType.endOfTrack, []),
        ),
    ]);
    range.writeTrackChunk(chunk);
    assert(result == [
        'M', 'T', 'r', 'k',
        0, 0, 0, 13,
        0x64,       0x93, 0x4C, 0x20,
        0x82, 0x2C,       0x4C, 0x00, // running status
        0x83, 0x10, 0xFF, 0x2F, 0x00,
    ]);
}

/**
`writeTrackEvent` encodes a single track event to binary data and `put`s it
into `output`.

If you want to encode using running status (meaning it leaves out the status
byte if this event's status byte is the same as the previous one), set
`runningStatus` to the previous event's status byte. Otherwise, set it to 0.

Params:
    output = the output range to send the bytes to
    event = the track event that is encoded to bytes
    runningStatus = the previous event's status byte; set to `0` if you don't
        care about saving space using running status
*/
void writeTrackEvent(T)(ref T output, ref const TrackEvent event, ubyte runningStatus)
if (isOutputRange!(T, ubyte)) {
    import mididi.def : getDataLength;

    output.writeVariableInt(event.deltaTime);
    if (auto midiEvent = event.asMIDIEvent()) {
        if (event.statusByte != runningStatus) {
            output.writeInt8(event.statusByte);
        }
        output.write(midiEvent.data[0 .. getDataLength(event.statusByte)]);
    } else if (auto sysExEvent = event.asSysExEvent()) {
        output.writeInt8(event.statusByte);
        output.writeVariableInt(cast(uint) sysExEvent.data.length);
        output.write(sysExEvent.data);
    } else if (auto metaEvent = event.asMetaEvent()) {
        assert(event.statusByte == 0xFF);
        output.writeInt8(0xFF);
        output.writeInt8(cast(ubyte) metaEvent.type);
        output.writeVariableInt(cast(uint) metaEvent.data.length);
        output.write(metaEvent.data);
    } else {
        assert(false, "unreachable");
    }
}

private:

void writeVariableInt(T)(ref T output, uint x)
in (x < (1 << 28)) {
    if (x < (1 << 7)) {
        output.put(cast(ubyte) x);
    } else if (x < (1 << 14)) {
        ubyte[2] bytes = [
            (1 << 7) | cast(ubyte) ((x >> 7) & 0x7F),
            (0 << 7) | cast(ubyte) ((x >> 0) & 0x7F),
        ];
        output.write(bytes[]);
    } else if (x < (1 << 21)) {
        ubyte[3] bytes = [
            (1 << 7) | cast(ubyte) ((x >> 14) & 0x7F),
            (1 << 7) | cast(ubyte) ((x >> 7) & 0x7F),
            (0 << 7) | cast(ubyte) ((x >> 0) & 0x7F),
        ];
        output.write(bytes[]);
    } else {
        ubyte[4] bytes = [
            (1 << 7) | cast(ubyte) ((x >> 21) & 0x7F),
            (1 << 7) | cast(ubyte) ((x >> 14) & 0x7F),
            (1 << 7) | cast(ubyte) ((x >> 7) & 0x7F),
            (0 << 7) | cast(ubyte) ((x >> 0) & 0x7F),
        ];
        output.write(bytes[]);
    }
}

// This test checks if variable integer writing works correctly.
unittest {
    void test(uint x, ubyte[] result) {
        auto local = result;
        auto sink = DelegateSink((scope const bytes) {
            assert(bytes.length <= local.length);
            assert(bytes == local[0 .. bytes.length]);
            local = local[bytes.length .. $];
        });
        sink.writeVariableInt(x);
        assert(local.length == 0);
    }

    test(0x000, [0]);
    test(0x40, [0x40]);
    test(0x7F, [0x7F]);
    test(0x80, [0x81, 0x00]);
    test(0x2000, [0xC0, 0x00]);
    test(0x3FFF, [0xFF, 0x7F]);
    test(0x4000, [0x81, 0x80, 0x00]);
    test(0x100000, [0xC0, 0x80, 0x00]);
    test(0x1FFFFF, [0xFF, 0xFF, 0x7F]);
    test(0x200000, [0x81, 0x80, 0x80, 0x00]);
    test(0x8000000, [0xC0, 0x80, 0x80, 0x00]);
    test(0xFFFFFFF, [0xFF, 0xFF, 0xFF, 0x7F]);
}

void writeInt32(T)(ref T output, uint x)
if (isOutputRange!(T, ubyte)) {
    import std.bitmanip : nativeToBigEndian;

    ubyte[4] bytes = nativeToBigEndian(x);
    output.write(bytes[]);
}

void writeInt16(T)(ref T output, ushort x)
if (isOutputRange!(T, ubyte)) {
    import std.bitmanip : nativeToBigEndian;

    ubyte[2] bytes = nativeToBigEndian(x);
    output.write(bytes[]);
}

void writeInt8(T)(ref T output, ubyte x)
if (isOutputRange!(T, ubyte)) {
    ubyte[1] bytes = [x];
    output.write(bytes[]);
}

void write(T)(ref T output, scope const ubyte[] bytes)
if (isOutputRange!(T, ubyte)) {
    static if (
        __traits(hasMember, output, "putMultiple") &&
        __traits(compiles, {
            ubyte[1] x = [0];
            output.putMultiple(x[]);
        })
    ) {
        output.putMultiple(bytes);
    } else {
        foreach (b; bytes) {
            output.put(b);
        }
    }
}
