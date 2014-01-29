window.Klatch = {
  ClientTag: 'klatch.js',
  ClientVersion: '0.0.1',

  Projection: {
    forSubtag: function (tag, subtag, f) {
      return function (projections, msg, postpone) {
        if (msg[tag] && msg[tag][subtag])
          f.call(this, projections, msg[tag][subtag], postpone, msg);
      };
    },

    forTag: function (tag, f) {
      return function (projections, msg, postpone) {
        if (msg[tag])
          f.call(this, projections, msg[tag], postpone);
      };
    }
  },

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

