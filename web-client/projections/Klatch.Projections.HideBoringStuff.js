Klatch.Projections.HideBoringStuff = function () { return {
  state: {},
  update: Klatch.Projection.forSubtag(
    'klatch.js', 'HideBoringStuff', function (_, data) {
      var id = Klatch.areaId(data);
      this.state[id] = !this.state[id];
    })
}};
