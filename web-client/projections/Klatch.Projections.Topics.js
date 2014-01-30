Klatch.Projections.Topics = function () { return {
  state: {},
  update: Klatch.Projection.forSubtag(
    'irc', 'Received', function (_, irc, _, msg) {
      if (irc.command == '332')
        this.state[msg.getAreaId(1)] = irc.trail;
    })
}};
