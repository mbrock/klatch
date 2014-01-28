Klatch.Projections.AreaMinimization = function () { return {
  state: {},
  update: function (_, msg) {
    var data;
    if (data = msg['klatch.js'])
      if (data.ToggleAreaMinimization) {
        source = data.ToggleAreaMinimization.area;
        this.state[source] = !this.state[source];
      }
  }
} };
