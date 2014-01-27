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
        areaMinimization: { },
        inhabitations: { }
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
      var message = Klatch.MessageModel(data);
      var messages = this.state.messages;
      var inhabitations = this.state.inhabitations;
      var newMessages = {};
      var update = {};
      var source;

      if (data.meta && data.meta.Replaying) {
        update.replaying = data.meta.Replaying.count;
        update.replayed = 0;
      }

      else if (data.meta && data.meta.Streaming) {
        update.replaying = update.replayed = 0;
      }

      else if ((data.irc && data.irc.Received) ||
               (data.socket && data.socket.Error)) {

        function save (source, message) {
          newMessages[source] = (messages[source] || []).concat(message);
        }

        if (data.irc) {
          var msg = data.irc.Received;
          var command = msg.command;

          if (command == 'PRIVMSG') {
            save(message.getNameForArea(), message);

          } else if (command == 'NICK') {
            for (var channelName in inhabitations)
              if (inhabitations[channelName].indexOf(msg.name) >= 0)
                save(channelName, message)

          } else {
            console.log(msg);
          }
        }

        if (data.socket && data.socket.Error)
          save(message.getNameForArea(), message);

        update.messages = $.extend({}, messages, newMessages);

        update.replayed = this.updateReplayCount(data.meta);
      }

      else if (data['klatch.js']) {
        update = this.handleClientEvent(data['klatch.js'], message);
      }

      else {
        update.replayed = this.updateReplayCount(data.meta);
      }

      this.setState(update);
    },

    handleClientEvent: function (data, message) {
      var messages = {};
      var areaMinimization = {};

      if (data.MarkAsRead) {
        source = data.MarkAsRead.area;
        if (this.state.messages[source])
          messages[source] = this.state.messages[source].concat(message);

      } else if (data.ToggleAreaMinimization) {
        source = data.ToggleAreaMinimization.area;
        areaMinimization[source] = !this.state.areaMinimization[source];

      } else {
        console.log("Unrecognized client event '%o': %o", data, message);
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
