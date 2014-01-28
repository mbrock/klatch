window.Klatch = {
  ClientTag: 'klatch.js',
  ClientVersion: '0.0.1',

  Projections: {},
  
  recordClientEvent: function (data) {
    var payload = {};
    payload[Klatch.ClientTag] = data;

    if (Klatch.projectionState('Online').online)
      return $.post('/api/command', JSON.stringify({
        event: { Record: payload }
      }));
    else {
      payload.synthetic = true;
      setTimeout(function () {
        Klatch.recordMessage(payload);
      }, 500);
    }
  },

  sendCommand: function (command) {
    return $.post('/api/command', JSON.stringify(command));
  }
};

