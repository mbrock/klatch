# Klatch

**Klatch** (license: AGPL3) is an ongoing attempt to create a happy IRC experience.  It's a kind of bouncer together with a special client that only works with this bouncer.  The reason is that the bouncer and the client don't communicate via the IRC protocol; they use HTTP instead, and the client is a modern JavaScript application.

A major focus of Klatch is to be smart about historical data.  To ensure high connectivity, an isolated process maintains the IRC server connections logging all messages into a persistent queue.  The idea is that we should be able to upgrade the complex user-facing parts of the system without disturbing the simple IRC communication.

The log-based design of Klatch makes it natural for new user interface features to function "retroactively," instead of only on the chat activity that happens after the feature is introduced.  Since the client is basically defined as a map & fold operation on the entire recorded history, we get a nice platform for experimenting with IRC client features.

## Dependencies

To install and run Klatch, you need

* the Haskell Platform;
* many Cabal packages which will be installed for you; and
* RabbitMQ.

If you have installed GHC and Cabal without installing the Haskell Platform, that's fine too, though you probably need newish versions.

You may want to look into using a "Cabal sandbox" to avoid installing all of Klatch's Haskell dependencies into your default GHC locations.

If you are running RabbitMQ with its default settings, you should be able to just run

    $ ./run-envoy.sh

in one terminal or screen, and

    $ ./run-embassy.sh

in another.  Now you can visit the URL announced by the "embassy" process.
