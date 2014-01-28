Klatch.Projections.Inhabitation = function () { return {
  state: {},

  forUserChannels: function (msg, f) {
    var user = msg.irc.Received.prefix.User.nick;
    for (var key in this.state)
      if (this.state[key].indexOf(user) != -1)
        f(key);
  },

  update: function (_, msg, postpone) {
    var commands = {
      QUIT: function (name, msg, postpone) {
        postpone(function () {
          for (var key in this.state)
            if (this.state[key].indexOf(name) != -1)
              this.state[key] = this.state[key].filter(
                function (x) { return x != name; });
        });
      },

      JOIN: function (name, msg) {
        var channel = msg.getChannelId();
        if (!this.state.hasOwnProperty(channel))
          this.state[channel] = [];

        this.state[channel].push(name);
      },

      PART: function (name, msg) {
        var channel = msg.getChannelId();

        if (!this.state.hasOwnProperty(channel))
          return;

        this.state[channel] = this.state[channel].filter(
          function (x) { return x != name; });
      }
    };

    var irc;
    if (irc = msg.irc ? msg.irc.Received : false) {
      if (commands.hasOwnProperty(irc.command)) {
        var nick = irc.prefix.User ? irc.prefix.User.nick : "[unknown]";
        commands[irc.command].call(this, nick, msg, postpone);
      }
    }
  }

} };
