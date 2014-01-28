Klatch.Projections.ReplayStatus = function () { return {
  state: {
    replaying: 0,
    replayed: 0,
  },

  update: function (_, msg) {
    if (msg.meta && msg.meta.Replaying) {
      this.state.replaying = msg.meta.Replaying.count;
      this.state.replayed = 0;
    }

    else if (msg.meta && msg.meta.Streaming)
      this.state.replaying = this.state.replayed = 0;

    else if (this.state.replaying && !msg.meta)
      this.state.replayed += 1;
  }
} };

