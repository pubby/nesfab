# NESFab

NESFab is a new programming language for creating NES games.

**Website:** http://pubby.games/nesfab.html

## License

- The compiler (files in `src/`) is licensed under GPL 3.0 (see `COPYING`).
- The NESFab standard library (files in `lib/`) is licensed under the Boost Software License 1.0 (See `lib/LICENSE_1_0.txt`).
- The NESFab examples (files in `examples/`) are licensed under the Boost Software License 1.0 (See `examples/LICENSE_1_0.txt`).

## Building

**Requirements:**
- [GCC Compiler](https://gcc.gnu.org/), supporting the C++20 standard.
- [Boost Library](https://www.boost.org/)
- [Make](https://www.gnu.org/software/make/)

The NESFab can be built in either debug mode, or release mode.
- Debug mode includes runtime sanity checks to ensure the compiler is working correctly.
- Release mode has no checks and is optimized for speed.

To build in debug mode, run:

    make debug

To build in release mode, run:

    make release

## Bug Reports

Post bug reports on the Github issues page and
include a [minimal reproducible example](https://en.wikipedia.org/wiki/Minimal_reproducible_example) if possible.

When testing bugs, use the debug-mode compiler. It gives far better diagnostics.

## Contributing

Small fixes are welcome and can be submitted as Github pull requests.
For anything larger, open a Github issue with what you'd like to do, and await approval before starting.
