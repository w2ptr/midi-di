/**


The implementation (and some of the documentation) is based on this specification:
https://www.cs.cmu.edu/~music/cmsip/readings/Standard-MIDI-file-format-updated.pdf

Authors:
    Wout Huynen
*/
module mididi.types;

// Copyright Wout Huynen 2021.
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE.txt or copy at
// https://www.boost.org/LICENSE_1_0.txt)

import mididi.def;

/**
`MIDI` represents the data of a complete MIDI file.
*/
struct MIDI {
    ///
    HeaderChunk headerChunk;

    ///
    TrackChunk[] trackChunks;
}

/**
A more general chunk type, which is essentially a tagged union of `HeaderChunk`
and `TrackChunk`.

Use `asHeaderChunk()` or `asTrackChunk()` to find out which one this is.
*/
struct Chunk {
    ///
    this(HeaderChunk chunk) @nogc nothrow pure @trusted {
        _type = Type.headerChunk;
        _headerChunk = chunk;
    }
    /// ditto
    this(TrackChunk chunk) @nogc nothrow pure @trusted {
        _type = Type.trackChunk;
        _trackChunk = chunk;
    }

    /**
    Returns:
        a pointer to the header chunk or `null` if this chunk is not a header
        chunk
    */
    inout(HeaderChunk)* asHeaderChunk() inout return @nogc nothrow pure @safe {
        if (_type == Type.headerChunk) {
            return (() inout @trusted => &_headerChunk)();
        }
        return null;
    }

    /**
    Returns:
        a pointer to the track chunk or `null` if this chunk is not a track
        chunk
    */
    inout(TrackChunk)* asTrackChunk() inout return @nogc nothrow pure @safe {
        if (_type == Type.trackChunk) {
            return (() inout @trusted => &_trackChunk)();
        }
        return null;
    }

private:
    enum Type {
        headerChunk,
        trackChunk,
    }

    Type _type;
    union {
        HeaderChunk _headerChunk;
        TrackChunk _trackChunk;
    }
}

/**
`HeaderChunk` is the first chunk in a MIDI file. It gives information about the
format of the other chunks.
*/
struct HeaderChunk {
    /**
    The track format (the possible values are given in the enum
    `mididi.def.TrackFormat`).

    Cast this to `ushort` to get the raw value, which should be either 0, 1 or
    2.
    */
    TrackFormat trackFormat;

    /**
    The number of tracks in the data.

    `nTracks` should always be 1 for if `trackFormat` is `TrackFormat.single`.
    */
    ushort nTracks;

    /**
    */
    TimeDivision division;
}

/**
*/
struct TimeDivision {
    /**
    `fromRawValue` creates a `TimeDivision` object from the raw encoding of
    this property in the header chunk.
    */
    static TimeDivision fromRawValue(ushort rawValue) @nogc nothrow pure @safe {
        return TimeDivision(rawValue);
    }
    /**
    */
    static TimeDivision fromFormat0(ushort ticksPerQuarterNote) @nogc nothrow pure @safe
    in (ticksPerQuarterNote <= 0x7FFF) {
        return TimeDivision(ticksPerQuarterNote);
    }
    /**
    */
    static TimeDivision fromFormat1(byte negativeSMPTEFormat, ubyte ticksPerFrame) @nogc nothrow pure @safe
    in (
        negativeSMPTEFormat == -24 || negativeSMPTEFormat == -25 ||
        negativeSMPTEFormat == -29 || negativeSMPTEFormat == -30
    ) {
        immutable byte2 = (() @trusted => *cast(ubyte*) &negativeSMPTEFormat)();
        return TimeDivision(
            (1 << 15) | (byte2 << 8) | ticksPerFrame,
        );
    }

    /// (read-only)
    @property ushort rawValue() const @nogc nothrow pure @safe {
        return _rawValue;
    }

    /**
    Returns the time division's format. If the format is `0`, then use
    `getTicksPerQuarterNote()`. If it is `1`, use `getNegativeSMPTEFormat()`
    and `getTicksPerFrame()`.

    Returns:
        `0` if the division's format is how many ticks make up a quarter note
        (bit 15 is 0) or `1` if it is ticks-per-frame (bit 15 is 1)
    */
    int getFormat() const @nogc nothrow pure @safe
    out (result; result == 0 || result == 1) {
        return (_rawValue >> 15) & 1;
    }

    /**
    Preconditions:
        `getFormat()` must be `0`
    */
    ushort getTicksPerQuarterNote() const @nogc nothrow pure @safe
    in (this.getFormat() == 0) {
        // get bits 14 through 0
        return _rawValue;
    }

    /**
    Returns:
        one of the four standard SMPTE formats (-24, -25, -29, -30),
        representing the negative of the number of frames/second
    Preconditions:
        `getFormat()` must be `1`
    */
    byte getNegativeSMPTEFormat() const @nogc nothrow pure @safe
    in (this.getFormat() == 1) {
        // get bits 14 through 8
        immutable bits = cast(ubyte) ((_rawValue >> 8) & 0b01111111);
        // sign extend because it's in 2's complement
        immutable value = bits | ((bits & 0b01000000) << 1);
        // interpret as 2's complement
        return (() @trusted => *cast(byte*) &value)();
    }

    /**
    Returns:
        the number of ticks per frame
    Preconditions:
        `getFormat()` must be `1`
    */
    ubyte getTicksPerFrame() const @nogc nothrow pure @safe
    in (this.getFormat() == 1) {
        // get bits 7 through 0
        return _rawValue & 0b11111111;
    }

private:
    this(ushort rawValue) @nogc nothrow pure @safe {
        _rawValue = rawValue;
    }
    ushort _rawValue;
}

/**
A track chunk is a chunk that stores the actual data. It contains a number of
track events.
*/
struct TrackChunk {
    ///
    TrackEvent[] events;
}

/**
A `TrackEvent` is a delta time coupled with either a MIDI event, a system
exclusive event or a meta event.

`TrackEvent` has an underlying union that uses the status byte to discriminate
(which cannot be changed after construction). Use `asMIDIEvent()`,
`asSysExEvent()` or `asMetaEvent()` to access the underlying values from the
union.

See the documentation of `mididi.def.isMIDIEvent()` to see what each type of
message means. Note: system exclusive events ARE also semantically system
common messages. The confusing thing is that all other system common messages
are syntactically MIDI events, but system exclusive events are not MIDI events.
*/
struct TrackEvent {
    /**
    Preconditions:
        `statusByte` must be a valid status byte for a MIDI event, and not for
        a system exclusive event
    */
    this(int deltaTime, ubyte statusByte, MIDIEvent event) @nogc nothrow pure @trusted
    in (isMIDIEvent(statusByte)) {
        this.deltaTime = deltaTime;
        _statusByte = statusByte;
        _midiEvent = event;
    }
    /**
    Preconditions:
        `statusByte` must be a valid status byte for a system exclusive event
    */
    this(int deltaTime, ubyte statusByte, SysExEvent event) @nogc nothrow pure @trusted
    in (isSysExEvent(statusByte)) {
        this.deltaTime = deltaTime;
        _statusByte = statusByte;
        _sysExEvent = event;
    }
    /**
    Preconditions:
        `statusByte` must be a MIDI event status byte (meaning its value must
        be `0xFF`)
    */
    this(int deltaTime, ubyte statusByte, MetaEvent event) @nogc nothrow pure @trusted
    in (isMetaEvent(statusByte)) {
        this.deltaTime = deltaTime;
        _statusByte = statusByte;
        _metaEvent = event;
    }

    /**
    `deltaTime` is the time between the previous event and this event.
    */
    int deltaTime;

    /**
    Returns:
        this event's status byte (see `TrackEvent`'s' documentation)
    Note:
        this property cannot be changed after construction
    */
    @property ubyte statusByte() const @nogc nothrow pure @safe {
        return _statusByte;
    }

    /**
    Returns:
        a pointer to the underlying `MIDIEvent`, `SysExEvent` or `MetaEvent`
        if the object is of that variety; `null` otherwise

    Note:
        do not reassign the `TrackEvent` object while still having a reference
        to the object in the union.
    */
    inout(MIDIEvent)* asMIDIEvent() inout return @nogc nothrow pure @safe {
        if (isMIDIEvent(_statusByte)) {
            return (() inout @trusted => &_midiEvent)();
        }
        return null;
    }
    /// ditto
    inout(SysExEvent)* asSysExEvent() inout return @nogc nothrow pure @safe {
        if (isSysExEvent(_statusByte)) {
            return (() inout @trusted => &_sysExEvent)();
        }
        return null;
    }
    /// ditto
    inout(MetaEvent)* asMetaEvent() inout return @nogc nothrow pure @safe {
        if (isMetaEvent(_statusByte)) {
            return (() inout @trusted => &_metaEvent)();
        }
        return null;
    }

private:
    ubyte _statusByte;
    union {
        MIDIEvent _midiEvent;
        SysExEvent _sysExEvent;
        MetaEvent _metaEvent;
    }
}

/// Here is an example of how to use `TrackEvent`.
@nogc nothrow pure @safe unittest {
    auto statusByte = cast(ubyte) ((ChannelMessageType.noteOn << 4) | 0x7);
    auto event = TrackEvent(
        200,
        statusByte,
        MIDIEvent(statusByte, [0x0F, 0x00]),
    );

    // get underlying value
    assert(event.asSysExEvent() is null);
    assert(event.asMetaEvent() is null);
    const message = event.asMIDIEvent();
    assert(message !is null);

    // MIDI event = either a channel message or a system message
    assert(message.isChannelMessage());
    assert(!message.isSystemMessage());

    // since it's a channel message, we can use these properties of the
    // `MIDIEvent` struct:
    assert(message.getChannelMessageType() == ChannelMessageType.noteOn);
    assert(message.getChannelNumber() == 0x7);
    assert(message.data == [0x0F, 0x00]);
}

/**
A MIDI event is any normal channel message or system message (except for system
exclusive events).

See also:
    `mididi.def.isMIDIEvent`
*/
struct MIDIEvent {
    ///
    ubyte statusByte;

    /**
    The data bytes in this message.

    The first bit of both bytes must be 0 and if the message type only needs
    one byte, the second byte should be `0x00`.
    */
    ubyte[2] data;

    /**
    */
    bool isChannelMessage() const @nogc nothrow pure @safe {
        return .isChannelMessage(statusByte);
    }
    /// ditto
    bool isSystemMessage() const @nogc nothrow pure @safe {
        return .isSystemMessage(statusByte);
    }

    /**
    These methods give the channel message type and the channel number (upper
    and lower four bits, respectively).

    Preconditions:
        this message must be a channel message, so `this.isChannelMessage()`
        must be true.
    See also:
        `mididi.def.ChannelMessageType`
    */
    ChannelMessageType getChannelMessageType() const @nogc nothrow pure @safe
    in (this.isChannelMessage()) {
        immutable t = cast(ubyte) (statusByte >> 4);
        return cast(ChannelMessageType) t;
    }
    /// ditto
    ubyte getChannelNumber() const @nogc nothrow pure @safe
    in (this.isChannelMessage()) {
        return statusByte & 0xF;
    }

    /**
    Preconditions:
        this message must be a system message, so `this.isSystemMessage()` must
        be true.
    See also:
        `mididi.def.SystemMessageType`
    */
    SystemMessageType getSystemMessageType() const @nogc nothrow pure @safe
    in (this.isSystemMessage()) {
        return cast(SystemMessageType) statusByte;
    }
}

/**
A system exclusive event is one unit ("packet") of a system exclusive message,
which can be used as an escape to emit any arbitrary bytes.

See also:
    `mididi.def.isSysExEvent`
*/
struct SysExEvent {
    /**
    The kind of system exclusive event. This can be either `systemExclusive`
    (`0xF0`) or `endOfExclusive` (`0xF7`).
    */
    SystemMessageType type;

    ///
    ubyte[] data;
}

/**
A meta event is an event that is not regular MIDI message, used to send meta
information. Not every meta event has to be supported or acted upon.

See Also:
    `mididi.def.isMetaEvent`; `mididi.def.MetaEventType`
*/
struct MetaEvent {
    ///
    MetaEventType type;

    // is usually 1-6 bytes, could also be more data depending if it's a
    // certain kind of meta event
    ///
    ubyte[] data;
}
