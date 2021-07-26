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

private:

/*
Returns the variable that was encoded in `bytes`.

If the variable-length int was invalid, then byteCount = 0.
*/
int readVariableInt(ubyte[] bytes, out size_t byteCount)
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
