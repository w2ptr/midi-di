/**


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
