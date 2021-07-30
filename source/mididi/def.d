/**
`mididi.def` contains definition enums and types important to MIDI files.

Authors:
    Wout Huynen
*/
module mididi.def;

// Copyright Wout Huynen 2021.
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE.txt or copy at
// https://www.boost.org/LICENSE_1_0.txt)

/**
`TrackFormat` is the MIDI data's format.

This format is given in the header chunk of a MIDI file.
*/
enum TrackFormat : ushort {
    /**
    If the format is `single`, that means the MIDI data consists of a header
    chunk followed by a single track chunk.
    */
    single = 0,

    /**
    If the format is `simultaneous`, that means the MIDI track data consists of
    multiple tracks, running in parallel.
    */
    simultaneous = 1,

    /**
    If the format is `sequential`, that means the MIDI track data consists of
    multiple tracks, running sequentially.
    */
    sequential = 2,
}

/**
A track event is one of the following:
- a MIDI event, meaning it carries around any message (channel or system),
except system exclusive messages;
- a system exclusive event, which offers an escape to transmit arbitrary bytes;
- a meta event, meaning it carries around other meta information.

Use `isMIDIEvent()`, `isSysExEvent()` and `isMetaEvent()` to find which kind of
event it is from its status byte.

See also:
    `mididi.types.TrackEvent`; `mididi.types.MIDIEvent`,
    `mididi.types.SysExEvent` and `mididi.types.MetaEvent`.
*/
bool isMIDIEvent(ubyte statusByte) @nogc nothrow pure @safe {
    return statusByte != 0xF0 && statusByte != 0xF7 && statusByte != 0xFF;
}
/// ditto
bool isSysExEvent(ubyte statusByte) @nogc nothrow pure @safe {
    return statusByte == 0xF0 || statusByte == 0xF7;
}
/// ditto
bool isMetaEvent(ubyte statusByte) @nogc nothrow pure @safe {
    return statusByte == 0xFF;
}

/**
`isChannelMessage()` and `isSystemMessage()` are used to find if a MIDI event
is a channel message or a system message.

In turn, a channel message can be either a channel voice message or a channel
mode message. A system message can be either a system common message or a
system realtime message.
*/
bool isChannelMessage(ubyte statusByte) @nogc nothrow pure @safe {
    immutable b = cast(ubyte) (statusByte >> 4);
    return b >= 0x8 && b <= 0xE;
}
/// ditto
bool isSystemMessage(ubyte statusByte) @nogc nothrow pure @safe {
    return statusByte >= 0xF0 && statusByte <= 0xFE; // TODO: 0xFF? 0xFE? which one?
}

/**
Returns:
    how many data bytes should be read for the message with status byte
    `statusByte`
Throws:
    `Exception` if `statusByte` is not a MIDI event
*/
size_t getDataLength(ubyte statusByte) pure @safe {
    import std.conv : text;

    if (isChannelMessage(statusByte)) {
        immutable t = cast(ChannelMessageType) (statusByte >> 4);
        with (ChannelMessageType) {
            if (t == programChange || t == channelPressure) {
                return 1;
            }

            if (
                t == noteOn || t == noteOff || t == polyphonicKeyPressure ||
                t == controlChangeOrMode || t == pitchWheelChange
            ) {
                return 2;
            }
        }
    } else if (isSystemMessage(statusByte)) {
        immutable t = cast(SystemMessageType) statusByte;
        with (SystemMessageType) {
            if (
                t == tuneRequest || t == timingClock || t == start ||
                t == continue_ || t == stop || t == activeSensing
            ) {
                return 0;
            }

            if (t == songSelect) {
                return 1;
            }

            if (t == songPositionPointer) {
                return 2;
            }
        }
    }

    throw new Exception(text(
        "Invalid channel message type for status byte ",
        statusByte,
    ));
}

/**
`ChannelMessageType` enumerates the possible types of channel messages.

The underlying value of this enumeration is the four upper bits of the status
byte (and always starts with a 1 bit). After all, a channel message has the
message type in the four upper bits and a channel identifier in the four lower
bits of the status byte.

The functions `isChannelVoiceMessage()` and `isChannelModeMessage()` need the
data bytes in addition to the status byte to identify if a channel message is a
voice message or a mode message, because some status byte values overlap (see
for example `ChannelMessageType.controlChangeOrMode`).

See also:
    `mididi.def.isChannelMessage`
*/
enum ChannelMessageType : ubyte {
    ///
    noteOff = 0x8,

    ///
    noteOn = 0x9,

    ///
    polyphonicKeyPressure = 0xA,

    /**
    NOTE: this kind of channel message can be either a Control Change message
    or a Channel Mode message, depending on the data bytes. Use
    `isChannelModeMessage()` to find out which it is.
    */
    controlChangeOrMode = 0xB,

    ///
    programChange = 0xC,

    ///
    channelPressure = 0xD,

    ///
    pitchWheelChange = 0xE,
}

/// ditto
bool isChannelVoiceMessage(ubyte statusByte, ubyte[2] dataBytes) @nogc nothrow pure @safe {
    immutable b = cast(ubyte) (statusByte >> 4);
    if (b == 0xB) {
        return dataBytes[0] <= 0x77;
    }
    return b >= 0x8 && b <= 0xE;
}
/// ditto
bool isChannelModeMessage(ubyte statusByte, ubyte[2] dataBytes) @nogc nothrow pure @safe {
    immutable b = cast(ubyte) (statusByte >> 4);
    return b == 0xB && dataBytes[0] >= 0x78;
}

/**
`SystemMessageType` enumerates the possible types of system message. Some of
these are system common messages, others are system realtime messages.

If for a `ubyte x` we have that `isSystemMessage(x)` is true, then it can be
safely cast to `SystemMessageType` using `cast(SystemMessageType) x`.

A system message is either a a "system common message" or a "system real-time
message". You can use `isSystemCommonMessage(x)` and
`isSystemRealTimeMessage(x)` to identify which is true, or compare to the enum
members to find out the exact message type.

See also:
    `mididi.def.isSystemMessage`
*/
enum SystemMessageType : ubyte {
    /**
    This event can give any type of information specific to the manufacturer.

    A system exclusive message can consist of several packets, where each
    packet is placed in one event. Each event then starts with the byte `0xF7`,
    and the final event is also terminated by `0xF7`.
    */
    systemExclusive = 0xF0,

    ///
    songPositionPointer = 0xF2,

    ///
    songSelect = 0xF3,

    ///
    tuneRequest = 0xF6,

    ///
    endOfExclusive = 0xF7,

    ///
    timingClock = 0xF8,

    ///
    start = 0xFA,

    ///
    continue_ = 0xFB,

    ///
    stop = 0xFC,

    ///
    activeSensing = 0xFE,
}

/// ditto
bool isSystemCommonMessage(SystemMessageType type) @nogc nothrow pure @safe {
    return isSystemCommonMessage(cast(ubyte) type);
}
/// ditto
bool isSystemCommonMessage(ubyte statusByte) @nogc nothrow pure @safe {
    return statusByte >= 0xF0 && statusByte <= 0xF7;
}
/// ditto
bool isSystemRealTimeMessage(SystemMessageType type) @nogc nothrow pure @safe {
     return isSystemRealTimeMessage(cast(ubyte) type);
}
/// ditto
bool isSystemRealTimeMessage(ubyte statusByte) @nogc nothrow pure @safe {
    return statusByte >= 0xF8 && statusByte <= 0xFE; // TODO: 0xFF? 0xFE? which one?
}

/**
`MetaEventType` enumerates the possible meta event types.

Cast to `ubyte` to get the underlying value.

Note:
    do not use a value of this type in a `final switch`, because it might have
    a value that is not identified in this enum, since values can always be
    added later.
See also:
    `mididi.def.isMetaEvent`
*/
enum MetaEventType : ubyte {
    /// 0x00
    sequenceNumber = 0x00,

    /// 0x01
    text = 0x01,

    /// 0x02
    copyright = 0x02,
    // TODO: this event should come in the first track, before any other event

    /// 0x03
    sequenceName = 0x03,

    /// 0x04
    instrumentName = 0x04,

    /// 0x05
    lyric = 0x05,

    /// 0x06
    marker = 0x06,

    /// 0x07
    cuePoint = 0x07,

    /// 0x20
    midiChannelPrefix = 0x20,

    /// 0x2F
    endOfTrack = 0x2F,
    // TODO: must come at end

    /// 0x51
    setTempo = 0x51,

    /// 0x54
    smpteOffset = 0x54,
    // TODO: must come before any event with nonzero delta-time

    /// 0x58
    timeSignature = 0x58,

    /// 0x59
    keySignature = 0x59,

    /// 0x7F
    sequencerSpecific = 0x7F,
}
