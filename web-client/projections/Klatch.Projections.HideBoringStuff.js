Klatch.Projections.HideBoringStuff = function () { return {
  state: {},
  update: Klatch.Projection.forSubtag(
    'klatch.js', 'HideBoringStuff', function (_, data) {
      this.state[data] = !this.state[data];
    })
}};
