/**
`mididi.reader` contains functions for reading the MIDI format.

Currently, the reader is compatible with the standard MIDI format 1.0 and
forward compatible with newer formats (in which case newer features will be
ignored, as the specification demands).

Authors:
    Wout Huynen
*/
module mididi.reader;

// Copyright Wout Huynen 2021.
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE.txt or copy at
// https://www.boost.org/LICENSE_1_0.txt)

import mididi.types;
import std.range.primitives : isInputRange, ElementType;
/**
`MIDIReader` is the raw MIDI reader object.

This can be used to read the standard MIDI chunk-by-chunk or event-by-event.

Constraints:
    `T` must be an input range of `ubyte`
*/
struct MIDIReader(T)
if (isInputRange!T && is(ElementType!T : const(ubyte))) {
    import std.range.primitives : empty, front, popFront;

    ///
    this(T range) {
        _input = range;
    }

    /**
    */
    HeaderChunk readHeaderChunk() {
        import mididi.def : TrackFormat;
        import std.conv : text;

        auto chunkType = cast(const(char[])) read(4);
        skip(4, "chunk type MThd");
        if (chunkType != "MThd") {
            throw new Exception("Not a header chunk: " ~ chunkType.idup);
        }

        immutable length = readInt32();
        if (length < 6) {
            throw new Exception("Header chunk must have length 6");
        }

        // see mididi.def.TrackFormat for the definitions
        immutable format = readInt16();
        if (format > TrackFormat.max) {
            throw new Exception("Header chunk format must be between 0 and " ~
                TrackFormat.max.stringof);
        }

        immutable tracks = readInt16();
        if (format == 0 && tracks != 1) {
            throw new Exception("If the header chunk's format is 0, the " ~
                "number of tracks given in the header chunk must be 1");
        }

        // bits: 15 | 14-8                  | 7-0             |
        //       ---+-----------------------+-----------------+
        //       0  |        ticks-per-quarter-note           |
        //       ---+-----------------------+-----------------+
        //       1  | negative SMPTE format | ticks-per-frame |
        immutable division = TimeDivision(readInt16());
        if (division.getFormat() == 1) {
            immutable smpteFormat = division.getNegativeSMPTEFormat();
            if (
                smpteFormat != -24 && smpteFormat != -25 &&
                smpteFormat != -29 && smpteFormat != -30
            ) {
                throw new Exception(text(
                    "If the division's format is 1, bits 14 through 8 must " ~
                        "be -24, -25, -29 or -30 in 2's complement, not ",
                    smpteFormat,
                ));
            }
        }

        skip(length - 6, "remaining header chunk bytes");

        return HeaderChunk(cast(TrackFormat) format, tracks, division);
    }

private:
    int readVariableInt() {
        auto bytes = read(4);
        size_t byteCount = 0;
        immutable result = .readVariableInt(bytes, byteCount);
        if (byteCount == 0) {
            throw new Exception("Expected variable integer");
        }
        skip(byteCount, "[impossible]");
        return result;
    }
    uint readInt32() {
        import std.bitmanip : bigEndianToNative;

        auto bytes = read(4);
        skip(4, "32-bit integer");
        assert(bytes.length == 4);
        ubyte[4] b = bytes[0 .. 4];
        return bigEndianToNative!uint(b);
    }
    ushort readInt16() {
        import std.bitmanip : bigEndianToNative;

        auto bytes = read(2);
        skip(2, "16-bit integer");
        assert(bytes.length == 2);
        ubyte[2] b = bytes[0 .. 2];
        return bigEndianToNative!ushort(b);
    }
    ubyte readInt8() {
        auto b = read(1);
        skip(1, "8-bit integer");
        return b[0];
    }

    // consumes n bytes, throws if not possible
    void skip(size_t n, string msg = "more bytes") {
        if (_peeked.length < n) {
            throw new Exception("Unexpected end of input, expected: " ~ msg);
        }
        _peeked = _peeked[n .. $];
        _index += n;
    }
    // reads at most n bytes without consuming
    ubyte[] read(size_t n)
    out (result; result.length <= n) {
        import std.algorithm.comparison : min;

        while (_peeked.length < n && !_input.empty) {
            _peeked ~= _input.front;
            _input.popFront();
        }
        return _peeked[0 .. min($, n)];
    }

    size_t _index = 0;
    T _input;
    ubyte[] _peeked;
}

private:

// This test checks if the header chunk parser works properly.
unittest {
    import mididi.def : TrackFormat;

    static MIDIReader!(ubyte[]) create(ubyte[] info) {
        ubyte[8] prefix = [
            'M', 'T', 'h', 'd',
            0x00, 0x00, 0x00, 0x06, // length 6 (as always)
        ];
        return MIDIReader!(ubyte[])(prefix ~ info);
    }

    try {
        auto r1 = create([
            0x00, 0x00, // format 0
            0x00, 0x01, // 1 track
            0x00, 0x60, // division: 0, 0, 0x60
        ]);
        auto h1 = r1.readHeaderChunk();
        assert(h1.trackFormat == TrackFormat.single);
        assert(h1.nTracks == 1);
        assert(h1.division.getFormat() == 0);
        assert(h1.division.rawValue == 0x0060);
        assert(h1.division.getTicksPerQuarterNote() == 0x60);

        auto r2 = create([
            0x00, 0x01, // format 1
            0xFF, 0xFF, // 65536 tracks
            0b1_1100111, 40, // division: 1, -25, 40
        ]);
        auto h2 = r2.readHeaderChunk();
        assert(h2.trackFormat == TrackFormat.simultaneous);
        assert(h2.nTracks == ushort.max);
        assert(h2.division.getFormat() == 1);
        assert(h2.division.getNegativeSMPTEFormat() == -25);
        assert(h2.division.getTicksPerFrame() == 40);
    } catch (Exception e) {
        assert(false, e.message);
    }
}

// This test checks if the header chunk errors correctly on invalid cases.
unittest {
    static void test(E = Exception)(ubyte[] bytes) {
        try {
            auto reader = MIDIReader!(ubyte[])(bytes);
            cast(void) reader.readHeaderChunk();
            assert(false, "Invalid header chunk but no exception thrown");
        } catch (E e) {}
    }

    // incomplete
    test([]);
    test(['M']);
    test(['M', 'T', 'h', 'd']);
    test(['M', 'T', 'h', 'd', 0x50]);
    test(['M', 'T', 'h', 'd', 0x00, 0x06]);

    // invalid length
    test(['M', 'T', 'h', 'd', 0x00, 0x05]);

    // invalid track format or number of tracks
    test([
        'M', 'T', 'h', 'd',
        0x00, 0x00, 0x00, 0x06, // length
        0x00, 0x03, // format
        0x00, 0x01, // tracks
        0x00, 0x60, // time division
    ]);
    test([
        'M', 'T', 'h', 'd',
        0x00, 0x00, 0x00, 0x06,
        0x02, 0x02,
        0x00, 0x01,
        0x00, 0x60,
    ]);
    test([
        'M', 'T', 'h', 'd',
        0x00, 0x00, 0x00, 0x06,
        0x00, 0x00,
        0x00, 0x02,
        0x00, 0x60,
    ]);

    // invalid time division
    test([
        'M', 'T', 'h', 'd',
        0x00, 0x00, 0x00, 0x06,
        0x00, 0x01,
        0xFF, 0xFF,
        0b1_1100110, 40,
    ]);
}

/*
Returns the variable that was encoded in `bytes`.

If the variable-length int was invalid, then byteCount = 0.
*/
int readVariableInt(const ubyte[] bytes, out size_t byteCount)
in (bytes.length > 0)
out (result; result >= 0 && result <= 0x0FFFFFFF)
out (result; byteCount <= 4) {
    import std.algorithm.comparison : min;

    // read at most 4 bytes of data, until the most significant bit (bit 7)
    // of the current byte is unset (=0)
    int result = 0;
    size_t i = 0;
    immutable count = min(4, bytes.length);
    while (i < count) {
        immutable b = cast(int) bytes[i];
        i++;
        result <<= 7;
        result += b & 0x7F;
        if ((b & 0x80) == 0) { // 7th bit == 0
            byteCount = i;
            return result;
        }
    }

    // not valid: int too large or bytes's length too small
    byteCount = 0;
    return 0;
}

// This test checks valid variable-length integer decoding.
unittest {
    size_t byteCount = 0;

    immutable x1 = readVariableInt([0x00], byteCount);
    assert(x1 == 0x00 && byteCount == 1);

    immutable x2 = readVariableInt([0x40], byteCount);
    assert(x2 == 0x40 && byteCount == 1);

    immutable x3 = readVariableInt([0x7F, 0x5E], byteCount);
    assert(x3 == 0x7F && byteCount == 1);

    immutable x4 = readVariableInt([0x81, 0x00], byteCount);
    assert(x4 == 0x80 && byteCount == 2);

    immutable x5 = readVariableInt([0xC0, 0x00, 0x80], byteCount);
    assert(x5 == 0x2000 && byteCount == 2);

    immutable x6 = readVariableInt([0xFF, 0x7F], byteCount);
    assert(x6 == 0x3FFF && byteCount == 2);

    immutable x7 = readVariableInt([0x81, 0x80, 0x00], byteCount);
    assert(x7 == 0x4000 && byteCount == 3);

    immutable x8 = readVariableInt([0xC0, 0x80, 0x00, 0xFF, 0x00, 0xFF], byteCount);
    assert(x8 == 0x100000 && byteCount == 3);

    immutable x9 = readVariableInt([0xFF, 0xFF, 0x7F, 0x00], byteCount);
    assert(x9 == 0x1FFFFF && byteCount == 3);

    immutable x10 = readVariableInt([0x81, 0x80, 0x80, 0x00, 0x80], byteCount);
    assert(x10 == 0x200000 && byteCount == 4);

    immutable x11 = readVariableInt([0xC0, 0x80, 0x80, 0x00], byteCount);
    assert(x11 == 0x8000000 && byteCount == 4);

    immutable x12 = readVariableInt([0xFF, 0xFF, 0xFF, 0x7F, 0x00], byteCount);
    assert(x12 == 0xFFFFFFF && byteCount == 4);
}

// This test checks invalid variable-length integer decoding.
unittest {
    size_t byteCount = 0;

    // too long
    readVariableInt([0xFF, 0xFF, 0xFF, 0xFF, 0x00], byteCount);
    assert(byteCount == 0);
    readVariableInt([0xFF, 0xFF, 0xFF, 0xFF], byteCount);
    assert(byteCount == 0);

    // too short (keeps going)
    readVariableInt([0x80], byteCount);
    assert(byteCount == 0);
    readVariableInt([0xCF, 0xFF], byteCount);
    assert(byteCount == 0);
    readVariableInt([0xFF, 0xFF, 0xFF], byteCount);
    assert(byteCount == 0);
}
