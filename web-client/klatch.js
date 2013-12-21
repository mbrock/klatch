/** @jsx React.DOM */

(function () {
  window.Klatch = {
    ClientTag: 'klatch.js',
    ClientVersion: '0.0.1',

    saveClientEvent: function (event) {
      event.version = Klatch.ClientVersion;
      return $.post('/', JSON.stringify({
        tag: 'SaveClientEvent',
        contents: [Klatch.ClientTag, JSON.stringify(event)]
      }));
    }
  };
})();
