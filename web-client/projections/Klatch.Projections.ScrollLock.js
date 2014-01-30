Klatch.Projections.ScrollLock = function () { return {
  state: {},
  update: Klatch.Projection.forSubtag(
    'klatch.js', 'ScrollLock', function (_, data) {
      var id = Klatch.areaId(data);
      this.state[id] = !this.state[id];
    })
}};
