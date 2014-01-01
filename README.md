# Klatch

**Klatch** (license: AGPL3) is an ongoing attempt to create a happy
IRC experience.

If you're familiar with the concept of an "IRC bouncer," Klatch
resembles one of those, one major difference being that the bouncer
and the client communicate via HTTP instead of IRC.

A major focus of Klatch is to be smart about historical data.  To
ensure high connectivity, an isolated process maintains the IRC server
connections logging all messages into a persistent queue.  The idea is
that we should be able to upgrade the complex user-facing parts of the
system without disturbing the simple IRC communication.

We also use an architecture similar to what's called *event sourcing*
in some circles, meaning that a simple log of events is the "source of
truth," instead of a structured database.  Booting a client involves
bringing it up to speed by "replaying" the event log.  This is a
somewhat experimental technique, but we think it is compelling.

## Dependencies

To install and run Klatch, you need

* the Haskell Platform;
* many Cabal packages which will be installed for you; and
* RabbitMQ.

If you have installed GHC and Cabal without installing the Haskell
Platform, that's fine too, though you probably need newish versions.

You may want to look into using a "Cabal sandbox" to avoid installing
all of Klatch's Haskell dependencies into your default GHC locations.

If you are running RabbitMQ with its default settings, you should be
able to just run

    $ ./run-envoy.sh

in one terminal or screen, and

    $ ./run-embassy.sh

in another.  Now you can visit the URL announced by the "embassy"
process.
