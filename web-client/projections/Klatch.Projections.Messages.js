Klatch.Projections.Messages = function () { return {
  state: {},
  update: function (projections, msg) {
    if (msg.socket && msg.socket.Error)
      this.save(msg.getNameForArea(), msg);

    if (!msg.irc || !msg.irc.Received) return;

    var command = msg.irc.Received.command;
    var inhabitation = projections.Inhabitation.state;
    var area = msg.getNameForArea();

    if (command == 'PRIVMSG')
      this.save(area, msg);

    else if (projections.HideBoringStuff.state[area])
      return;

    else if (command == 'NICK') {
      for (var channelName in inhabitation)
        if (inhabitation[channelName].indexOf(msg.name) >= 0)
          this.save(channelName, msg)

    } else if (command == 'JOIN' || command == 'PART')
      this.save(msg.getChannelId(), msg);

    else if (command == 'QUIT')
      projections.Inhabitation.forUserChannels(msg, function (area) {
        this.save(area, msg);
      }.bind(this));
  },

  save: function (source, msg) {
    if (!this.state.hasOwnProperty(source))
      this.state[source] = [];
    this.state[source].push(msg);
  }
} };

