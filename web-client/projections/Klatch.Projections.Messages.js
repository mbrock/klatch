Klatch.Projections.Messages = function () { return {
  state: {},
  update: function (projections, msg) {
    if (msg.socket && msg.socket.Error)
      this.save(msg.getAreaDescriptor(), msg);

    if (!msg.irc || !msg.irc.Received) return;

    var command = msg.irc.Received.command;
    var inhabitation = projections.Inhabitation.state;
    var area = msg.getAreaDescriptor();

    if (command == 'PRIVMSG')
      this.save(area, msg);

    else if (command == 'NICK') {
      for (var areaId in inhabitation)
        if (inhabitation[areaId].indexOf(msg.name) >= 0)
          this.save(Klatch.fromAreaId(areaId), msg)

    } else if (command == 'JOIN' || command == 'PART')
      this.save(area, msg);

    else if (command == 'QUIT')
      projections.Inhabitation.forUserChannels(msg, function (area) {
        this.save(area, msg);
      }.bind(this));
  },

  save: function (areaDescriptor, msg) {
    var areaId = Klatch.areaId(areaDescriptor);
    if (!this.state.hasOwnProperty(areaId))
      this.state[areaId] = [];
    this.state[areaId].push(msg);
  }
} };

