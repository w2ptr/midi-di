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

