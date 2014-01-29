Klatch.Projections.HideBoringStuff = function () { return {
  state: {},
  update: function (_, msg) {
    var data;
    if (data = msg['klatch.js']) {
      if (data.HideBoringStuff) {
        source = data.HideBoringStuff;
        this.state[source] = this.state[source] ? false : true;
      }
    }
  }
} };
