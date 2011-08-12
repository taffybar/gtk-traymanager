This package wraps the venerable eggtraymanager code, which implements
the freedesktop.org
[system tray protocol](http://standards.freedesktop.org/systemtray-spec/systemtray-spec-0.2.html).
The code seems to have originated somewhere in the GNOME project a
long time ago; I took it from
[trayer-srg](https://github.com/sargon/trayer-srg/).  The only change
I made was to cast a few `long` variables in parameters to `uint64_t`
to make interoperability with Haskell easier.

By itself, this code is not a system tray widget.  It is, however, an
integral part of one.  This package exports a simple GObject (for use
with gtk2hs) that emits events when system tray events occur.  A
system tray widget would listen for these events and respond.  See the
haddock documentation for details.

