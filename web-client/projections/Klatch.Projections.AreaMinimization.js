Klatch.Projections.AreaMinimization = function () { return {
  state: {},
  update: Klatch.Projection.forSubtag(
    'klatch.js', 'ToggleAreaMinimization', function (_, data) {
      this.state[data.area] = !this.state[data.area];
    })
}};
