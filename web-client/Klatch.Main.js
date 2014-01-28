/** @jsx React.DOM */

setTimeout(function () {
  var Viewer = window.Klatch.Viewer;
  var viewer = <Viewer />;

  (function downloadEvents () {
    var source = new EventSource("/api/events");
    source.onmessage = function (e) {
      var data = JSON.parse(e.data);
      viewer.recordMessage(data);
    };
  })();

  React.renderComponent(viewer, log);
}, 0);
