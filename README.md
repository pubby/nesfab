# NESFab

NESFab is a new programming language for creating NES games.

- **Website:** http://pubby.games/nesfab.html
- **Documentation:** http://pubby.games/nesfab/doc.html
- **Discord:** https://discord.gg/RUrYmC5ZeE

## License

- The NESFab compiler (`src/`) is licensed under GPL 3.0 (see `COPYING`).
- The NESFab standard library (`lib/`), examples (`examples/`), and documentation (`doc/`) are licensed under the Boost Software License 1.0 (See `lib/LICENSE_1_0.txt`).

TL;DR: If you make a game using NESFab, your code can remain private, and you do not need to include attribution in your binary release.

## Building

**Requirements:**
- [GCC Compiler](https://gcc.gnu.org/), supporting the C++20 standard.
- [Boost Library](https://www.boost.org/) (You may also need the `libboost-program-options-dev` package)
- [Make](https://www.gnu.org/software/make/)

NESFab can be built in either debug mode, or release mode.
- Debug mode includes runtime sanity checks to ensure the compiler is working correctly.
- Release mode has no checks and is optimized for speed.

To build in debug mode, run:

    make debug

To build in release mode, run:

    make release

By default, the compiler builds executables for the x86-64 architecture.
To target others, specify a blank `ARCH` flag:

    make ARCH= release

## Bug Reports

Post bug reports on the Github issues page and
include a [minimal reproducible example](https://en.wikipedia.org/wiki/Minimal_reproducible_example) if possible.

When testing bugs, use the debug-mode compiler. It gives far better diagnostics.

## Contributing

Small fixes are welcome and can be submitted as Github pull requests.
For anything larger, open a Github issue prefixed with "Contrib Request:" and what you'd like to do, and await approval before starting.

## Made Something?

If you made a project using NESFab, please get in touch! It would be great to hear about.
