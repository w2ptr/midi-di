/**


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
    ///
    ushort rawValue;

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
        return (rawValue >> 15) & 1;
    }

    /**
    Preconditions:
        `getFormat()` must be `0`
    */
    ushort getTicksPerQuarterNote() const @nogc nothrow pure @safe
    in (this.getFormat() == 0) {
        // get bits 14 through 0
        return rawValue;
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
        immutable bits = cast(ubyte) ((rawValue >> 8) & 0b01111111);
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
        return rawValue & 0b11111111;
    }
}

