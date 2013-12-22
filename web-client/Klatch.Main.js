/** @jsx React.DOM */

$(function () {
  var Viewer = Klatch.Viewer;
  var viewer = <Viewer />;

  (function downloadEvents () {
    var source = new EventSource("/api/events");
    source.onmessage = function (e) {
      var data = JSON.parse(e.data);
      viewer.recordMessage(data);
    };
  })();

  React.renderComponent(viewer, log);
});
