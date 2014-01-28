Klatch.Projections.Topics = function () { return {
  state: {},
  update: function (_, msg) {
    var irc;
    if (irc = (msg.irc && msg.irc.Received))
      if (irc.command == '332')
        this.state[msg.getChannelId(1)] = irc.trail;
  }
} };
