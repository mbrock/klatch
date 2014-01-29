window.Klatch = {
  ClientTag: 'klatch.js',
  ClientVersion: '0.0.1',

  Projections: {},

  recordClientEvent: function (data, tag) {
    var payload = {};
    payload[tag || Klatch.ClientTag] = data;

    console.log("Client event: %o", payload);

    if (Klatch.projectionState('Online').online)
      return $.post('/api/command', JSON.stringify({
        event: { Record: payload }
      }));
    else {
      payload.synthetic = true;
      setTimeout(function () {
        Klatch.recordMessage(payload);
      }, 0);
    }
  },

  sendCommand: function (command) {
    return $.post('/api/command', JSON.stringify(command));
  }
};

