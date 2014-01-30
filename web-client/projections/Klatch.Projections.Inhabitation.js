Klatch.Projections.Inhabitation = function () { return {
  state: {},

  forUserChannels: function (msg, f) {
    var user = msg.irc.Received.prefix.User;
    if (!user) return;
    for (var key in this.state)
      if (this.state[key].indexOf(user) != -1)
        f(Klatch.fromAreaId(key));
  },

  update: Klatch.Projection.forSubtag(
    'irc', 'Received', function (_, irc, postpone, msg) {
      var commands = {
        QUIT: function (name, msg) {
          postpone(function () {
            for (var key in this.state)
              if (this.state[key].indexOf(name) != -1)
                this.state[key] = this.state[key].filter(
                  function (x) { return x != name; });
          });
        },

        JOIN: function (name, msg) {
          var areaId = msg.getAreaId();
          if (!this.state.hasOwnProperty(areaId))
            this.state[areaId] = [];

          this.state[areaId].push(name);
        },

        PART: function (name, msg) {
          var areaId = msg.getAreaId();

          if (!this.state.hasOwnProperty(areaId))
            return;

          this.state[areaId] = this.state[areaId].filter(
            function (x) { return x != name; });
        }
      };

      if (commands.hasOwnProperty(irc.command)) {
        var nick = irc.prefix.User ? irc.prefix.User.nick : "[unknown]";
        commands[irc.command].call(this, nick, msg);
      }
    })
}};
