Klatch.Projections.ScrollLock = function () { return {
  state: {},
  update: Klatch.Projection.forSubtag(
    'klatch.js', 'ScrollLock', function (_, data) {
      debugger;
      this.state[data] = !this.state[data];
    })
}};
