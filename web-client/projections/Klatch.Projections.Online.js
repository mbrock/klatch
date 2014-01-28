Klatch.Projections.Online = function () { return {
  state: { online: false, messagesHandled: 0 },
  update: function (_, msg) {
    ++this.state.messagesHandled;
    if (msg.meta && msg.meta.hasOwnProperty('Online'))
      this.state.online = msg.meta.Online;
  }
} };
