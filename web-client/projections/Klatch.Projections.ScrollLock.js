Klatch.Projections.ScrollLock = function () { return {
  state: {},
  update: function (_, msg) {
    var data;
    if (data = msg['klatch.js']) {
      if (data.ScrollLock) {
        source = data.ScrollLock;
        this.state[source] = this.state[source] ? false : true;
      }
    }
  }
} };
