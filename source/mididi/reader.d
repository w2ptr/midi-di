/**
`mididi.reader` contains functions for reading the MIDI format.

Currently, the reader is compatible with the standard MIDI format 1.0 and
forward compatible with newer formats (in which case newer features will be
ignored, as the specification demands).

The implementation (and some of the documentation) is based on this specification:
https://www.cs.cmu.edu/~music/cmsip/readings/Standard-MIDI-file-format-updated.pdf

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

    /**
    */
    TrackChunk readTrackChunk() {
        import std.conv : text;

        auto chunkType = cast(const(char[])) read(4);
        skip(4, "chunk type MTrk");
        if (chunkType != "MTrk") {
            throw new Exception("Not a track chunk: " ~ chunkType.idup);
        }

        immutable length = readInt32();
        immutable endIndex = _index + length;

        TrackEvent[] events;
        ubyte previousStatus = 0;
        bool inSysExPacket = false;
        while (_index != endIndex) {
            if (_index > endIndex) {
                throw new Exception(text(
                    "Expected track events for ", length, " bytes, but event ",
                    events.length, " stopped at byte ", _index + 1,
                ));
            }

            auto event = readTrackEvent(previousStatus, inSysExPacket);
            previousStatus = event.statusByte;
            events ~= event;
        }

        if (events.length == 0) {
            throw new Exception("At least one track event must be present " ~
                "in a track chunk");
        }

        if (inSysExPacket) {
            throw new Exception("Unterminated system exclusive packet");
        }

        return TrackChunk(events);
    }

    // TODO: should probably be public?
    private TrackEvent readTrackEvent(ubyte prevStatus, ref bool inSysExPacket) {
        import mididi.def : getDataLength, isMIDIEvent, isMetaEvent,
            isSysExEvent, MetaEventType, SystemMessageType;
        import std.conv : text;

        // <MTrk event> = <delta-time><event>
        // <event> = <MIDI event> | <sysex event> | <meta event>
        
        // <MIDI event> = [1tttnnnn] 0xxxxxxx 0yyyyyyy
        // if the status byte starts with a 0, use the previous one.
        // <sysex event> = F0 <length> <data> | F7 <length> <data>
        // <meta event> = FF <type> <length> <data>

        immutable deltaTime = readVariableInt();
        const nextByte = read(1);
        if (nextByte.length != 1) {
            throw new Exception("Input data ended at start of track event");
        }

        ubyte statusByte;
        if (((nextByte[0] >> 7) & 1) == 0) {
            if (isMIDIEvent(prevStatus)) { // running status
                statusByte = prevStatus;
            } else {
                throw new Exception(text(
                    "Running status does not work for non-midi events, but " ~
                    "the previous status byte is one: ", prevStatus,
                ));
            }
        } else {
            skip(1, "status byte");
            statusByte = nextByte[0];
        }

        if (!isSysExEvent(statusByte) && inSysExPacket) { // invalid
            throw new Exception(
                "No non-system-exclusive event may be emitted until the " ~
                "last system exclusive event",
            );
        }

        if (isMIDIEvent(statusByte)) {
            immutable dataLength = getDataLength(statusByte);
            auto varData = read(dataLength);
            skip(dataLength, "midi event data bytes");
            assert(varData.length == dataLength);
            ubyte[2] data = [0, 0];
            data[0 .. dataLength] = varData[];
            return TrackEvent(
                deltaTime,
                statusByte,
                MIDIEvent(statusByte, data),
            );
        } else if (isSysExEvent(statusByte)) {
            immutable type = cast(SystemMessageType) statusByte;
            immutable length = readVariableInt();
            auto data = read(cast(size_t) length);
            skip(cast(size_t) length, "system message data bytes");
            assert(data.length == length);

            if (inSysExPacket && type == SystemMessageType.systemExclusive) {
                throw new Exception(
                    "Cannot start a system exclusive packet while another " ~
                    "one is not terminated yet",
                );
            }

            if (type == SystemMessageType.systemExclusive) {
                // start of a packet
                inSysExPacket = true;
            }
            if (data.length > 0 && data[$ - 1] == 0xF7) {
                // end of a packet
                inSysExPacket = false;
            }

            return TrackEvent(
                deltaTime,
                statusByte,
                SysExEvent(type, length, data),
            );
        } else if (isMetaEvent(statusByte)) {
            immutable metaEventType = cast(MetaEventType) readInt8();
            immutable length = readVariableInt();
            auto data = read(cast(size_t) length);
            skip(cast(size_t) length, "meta event data bytes");

            return TrackEvent(
                deltaTime,
                0xFF,
                MetaEvent(metaEventType, length, data),
            );
        } else {
            throw new Exception(text(
                "Unrecognized status byte: ", statusByte,
            ));
        }
    }

    /**
    Returns:
        whether the reader has no more content to read.
    */
    bool isFinished() const @nogc nothrow pure @safe {
        return _peeked.length == 0;
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
}

// This test checks if the header chunk parser errors correctly on invalid
// cases.
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

// This test checks if the track chunk parser works properly (example taken
// from paper, see module comment).
unittest {
    import mididi.def : ChannelMessageType, MetaEventType;

    auto reader = MIDIReader!(ubyte[])([
        'M', 'T', 'r', 'k', // chunk type
        0x00, 0x00, 0x00, 0x3B, // length (59)

        0x00, 0xFF, 0x58, 0x04, 0x04, 0x02, 0x18, 0x08, // time signature (0)
        0x00, 0xFF, 0x51, 0x03, 0x07, 0xA1, 0x20, // tempo
        0x00, 0xC0, 0x05,
        0x00, 0xC1, 0x2E,
        0x00, 0xC2, 0x46, // (4)
        0x00, 0x92, 0x30, 0x60, // (5)
        0x40, 0x3C, 0x60, // running status (6)
        0x60, 0x91, 0x43, 0x40,
        0x60, 0x90, 0x4C, 0x20,
        0x81, 0x40, 0x82, 0x30, 0x40, // two-byte delta-time (9)
        0x00, 0x3C, 0x40, // running status
        0x00, 0x81, 0x43, 0x40,
        0x00, 0x80, 0x4C, 0x40,
        0x00, 0xFF, 0x2F, 0x00, // end of track
    ]);
    auto track = reader.readTrackChunk();

    assert(reader.isFinished());
    assert(track.events.length == 14);

    const event1 = track.events[0];
    assert(event1.deltaTime == 0x00);
    assert(event1.asMIDIEvent() is null);
    assert(event1.asSysExEvent() is null);
    const metaEvent1 = event1.asMetaEvent();
    assert(metaEvent1 !is null);
    assert(metaEvent1.type == MetaEventType.timeSignature);
    assert(metaEvent1.data == [0x04, 0x02, 0x18, 0x08]);
    
    const event2 = track.events[4];
    assert(event2.deltaTime == 0x00);
    const midiEvent2 = event2.asMIDIEvent();
    assert(midiEvent2 !is null);
    assert(midiEvent2.getChannelMessageType() == ChannelMessageType.programChange);
    assert(midiEvent2.getChannelNumber() == 2);

    const event3 = track.events[5];
    assert(event3.deltaTime == 0x00);
    const midiEvent3 = event3.asMIDIEvent();
    assert(midiEvent3 !is null);
    assert(midiEvent3.getChannelMessageType() == ChannelMessageType.noteOn);
    assert(midiEvent3.getChannelNumber() == 2);

    const event4 = track.events[6];
    assert(event4.deltaTime == 0x40);
    const midiEvent4 = event4.asMIDIEvent();
    assert(midiEvent4 !is null);

    assert(midiEvent4.getChannelMessageType() == midiEvent3.getChannelMessageType());
    assert(midiEvent4.getChannelNumber() == midiEvent3.getChannelNumber());

    const event5 = track.events[9];
    assert(event5.deltaTime == 192);
}

// This test checks system exclusive messages.
// See the test below for invalid system exclusive messages.
unittest {
    static void testNoErrors(ubyte[] bytes) {
        ubyte[4] start = [
            'M', 'T', 'r', 'k'
        ];
        auto reader = MIDIReader!(ubyte[])(start ~ bytes);
        cast(void) reader.readTrackChunk();
        assert(reader.isFinished());
    }

    testNoErrors([
        0, 0, 0, 13, // length
        0x00, 0xF0, 0x01, 0xFF,
        0x00, 0xF7, 0x02, 0xFF, 0xF7,
        0x00, 0xFF, 0x2F, 0x00, // end of track
    ]);
    testNoErrors([
        0, 0, 0, 8,
        0x00, 0xF0, 0x01, 0xF7,
        0x00, 0xFF, 0x2F, 0x00,
    ]);
    
    // system messages
    testNoErrors([
        0, 0, 0, 6,
        0x00, 0xFA,
        0x00, 0xFF, 0x2F, 0x00, // end of track
    ]);
}

// This test checks whether the functions correctly differentiate between
// control change messages and channel mode messages.
// (Arguably, this test could also be in `mididi.def`.)
unittest {
    import mididi.def : isChannelModeMessage, isChannelVoiceMessage;

    auto reader = MIDIReader!(ubyte[])([
        'M', 'T', 'r', 'k',
        0, 0, 0, 12,
        0x00, 0xB4, 0x77, 0x7F, // channel voice message
        0x00, 0xB4, 0x78, 0x00, // mode voice message
        0x00, 0xFF, 0x2F, 0x00,
    ]);
    auto track = reader.readTrackChunk();

    const midiEvent1 = track.events[0].asMIDIEvent();
    assert(midiEvent1.isChannelMessage());
    assert(isChannelVoiceMessage(midiEvent1.statusByte, midiEvent1.data));
    assert(!isChannelModeMessage(midiEvent1.statusByte, midiEvent1.data));
    assert(midiEvent1.getChannelNumber() == 4);

    const midiEvent2 = track.events[1].asMIDIEvent();
    assert(midiEvent2.isChannelMessage());
    assert(!isChannelVoiceMessage(midiEvent2.statusByte, midiEvent2.data));
    assert(isChannelModeMessage(midiEvent2.statusByte, midiEvent2.data));
    assert(midiEvent2.getChannelNumber() == 4);
}

// This test checks if the track chunk parser errors correctly on invalid
// cases when parsing individual track chunks.
unittest {
    static void test(E = Exception)(ubyte[] bytes) {
        auto reader = MIDIReader!(ubyte[])(bytes);

        try {
            cast(void) reader.readTrackChunk();
            assert(false, "Invalid track chunk but no exception thrown");
        } catch (Exception e) {}
    }

    // incomplete
    test([]);
    test(['M', 'T', 'r', 'k']);
    test(['M', 'T', 'r', 'k', 0x00]);

    // there must be >= 1 event
    test(['M', 'T', 'r', 'k', 0, 0, 0, 0]);

    // fallthrough only works for midi events, not for other events
    test([
        'M', 'T', 'r', 'k',
        0, 0, 0, 5,
        0xFF, 0x01, 0x00, // e.g. for a meta event
        0x00, 0x00,
    ]);

    // fail on invalid sysex messages
    test([
        'M', 'T', 'r', 'k',
        0, 0, 0, 13,
        0x00, 0xF0, 0x01, 0xFF,
        0x00, 0xF7, 0x02, 0xFF, 0xFF,
        0x00, 0xFF, 0x2F, 0x00, // end of track
    ]);
    test([
        'M', 'T', 'r', 'k',
        0, 0, 0, 4,
        0x00, 0xF0, 0x01, 0xFF,
    ]);
    test([
        'M', 'T', 'r', 'k',
        0, 0, 0, 12,
        0x00, 0xF0, 0x01, 0xFF,
        0x00, 0xF0, 0x01, 0xF7,
        0x00, 0xFF, 0x2F, 0x00, // end of track
    ]);

    // wrong length for chunk (too long)
    test([
        'M', 'T', 'r', 'k',
        0x00, 0x00, 0x00, 20, // (1 too long)
        0x00, 0xFF, 0x58, 0x04, 0x04, 0x02, 0x18, 0x08,
        0x00, 0xFF, 0x51, 0x03, 0x07, 0xA1, 0x20,
        0x00, 0xFF, 0x2F, 0x00, // end of track
    ]);

    // wrong length for certain messages
    test([
        'M', 'T', 'r', 'k',
        0x00, 0x00, 0x00, 0x04,
        0x00, 0xF2, 0x00, // "song position pointer"
    ]);
}

/*
Returns the variable that was encoded in `bytes`.

If the variable-length int was invalid, then byteCount = 0.
*/
int readVariableInt(const ubyte[] bytes, out size_t byteCount)
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
    readVariableInt([], byteCount);
    assert(byteCount == 0);
    readVariableInt([0x80], byteCount);
    assert(byteCount == 0);
    readVariableInt([0xCF, 0xFF], byteCount);
    assert(byteCount == 0);
    readVariableInt([0xFF, 0xFF, 0xFF], byteCount);
    assert(byteCount == 0);
}
