/** @jsx React.DOM */

(function () {
  var Replaying = Klatch.Replaying;
  var AreaSplitter = Klatch.AreaSplitter;

  Klatch.Viewer = React.createClass({
    getInitialState: function () {
      return {
        replaying: 0,
        replayed: 0,
        messages: { },
        areaMinimization: { }
      };
    },

    render: function () {
      if (this.state.replaying > 0) {
        var progress = this.state.replayed / this.state.replaying;
        return <Replaying progress={progress} />;
      } else
        return <AreaSplitter messages={this.state.messages}
                             areaMinimization={this.state.areaMinimization} />;
    },

    updateReplayCount: function (isMetamessage) {
      if (this.state.replaying && !isMetamessage)
        return this.state.replayed + 1;
      else
        return this.state.replayed;
    },

    recordMessage: function (data) {
      var message = new Klatch.MessageModel(data);
      var messages = this.state.messages;
      var newMessages = {};
      var update = {};
      var source;

      if (message.payload.tag === 'Replaying') {
        update.replaying = message.contents;
        update.replayed = 0;
      }

      else if (message.payload.tag === 'Streaming') {
        update.replaying = update.replayed = 0;
      }

      else if (message.payload.tag === 'Received') {
        source = message.getNameForArea();

        newMessages[source] = (messages[source] || []).concat(message);
        update.messages = $.extend({}, messages, newMessages);

        update.replayed = this.updateReplayCount(message.isMeta);
      }

      else if (message.payload.tag === 'ClientEvent') {
        update = this.handleClientEvent(message.data.tag, message);
      }

      else {
        update.replayed = this.updateReplayCount(message.isMeta);
      }

      var nextState = $.extend({}, this.state, update);
      this.setState(nextState);
    },

    handleClientEvent: function (tag, message) {
      var messages = {};
      var areaMinimization = {};

      if (tag === 'mark-as-read') {
        source = message.data.area;
        if (this.state.messages[source])
          messages[source] = this.state.messages[source].concat(message);

      } else if (tag === 'toggle-area-minimization') {
        source = message.data.area;
        areaMinimization[source] = !this.state.areaMinimization[source];

      } else {
        console.log("Unrecognized client event '%o': %o", tag, message);
      }

      return {
        messages:
          $.extend({}, this.state.messages, messages),

        areaMinimization:
          $.extend({}, this.state.areaMinimization, areaMinimization),

        replayed: this.updateReplayCount(false)
      };
    }
  });
})();
