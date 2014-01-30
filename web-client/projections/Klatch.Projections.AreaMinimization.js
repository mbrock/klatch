Klatch.Projections.AreaMinimization = function () { return {
  state: {},
  update: Klatch.Projection.forSubtag(
    'klatch.js', 'ToggleAreaMinimization', function (_, data) {
      var id = Klatch.areaId(data);
      this.state[id] = !this.state[id];
    })
}};
