Klatch.Projections.SetTheme = function () { return {
  state: "//swa.sh/klatch/nice",
  update: Klatch.Projection.forSubtag(
    'klatch.js', 'SetTheme', function (_, uri) {
      this.state = uri;
    })
}};
