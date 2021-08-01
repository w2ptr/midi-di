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
