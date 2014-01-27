/** @jsx React.DOM */

(function () {
  var Replaying = Klatch.Replaying;
  var AreaSplitter = Klatch.AreaSplitter;

  var Inhabitation = {
    QUIT: function (inhabitation, name, msg, save) {
      for (var key in inhabitation)
        if (inhabitation[key].indexOf(name) != -1) {
          save(key);

          inhabitation[key] = inhabitation[key].filter(
            function (x) { return x != name; });
        }
    },

    JOIN: function (inhabitation, name, msg, save) {
      var channel = msg.getChannelId();
      if (!inhabitation.hasOwnProperty(channel))
        inhabitation[channel] = [];

      inhabitation[channel].push(name);

      save(channel);
    },

    PART: function (inhabitation, name, msg, save) {
      var channel = msg.getChannelId();

      if (!inhabitation.hasOwnProperty(channel))
        return;

      save(channel);

      inhabitation[channel] = inhabitation[channel].filter(
        function (x) { return x != name; });
    },

    handle: function (inhabitation, msg, save) {
      var irc = msg.irc.Received;
      if (this.hasOwnProperty(irc.command)) {
        var nick = irc.prefix.User ? irc.prefix.User.nick : "[unknown]";
        this[irc.command](inhabitation, nick, msg, save);
        return true;
      }

      return false;
    }
  };

  Klatch.Viewer = React.createClass({
    getInitialState: function () {
      return {
        replaying: 0,
        replayed: 0,
        messages: { },
        areaMinimization: { },
        inhabitation: { }
      };
    },

    render: function () {
      if (this.state.replaying > 0) {
        var progress = this.state.replayed / this.state.replaying;
        return <Replaying progress={progress} />;
      } else
        return <AreaSplitter messages={this.state.messages}
                             areaMinimization={this.state.areaMinimization}
                             online={this.state.online} />;
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
      var inhabitation = this.state.inhabitation;
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

      else if (data.meta && (data.meta.hasOwnProperty('Online'))) {
        update.online = data.meta.Online;
        console.log(update);
      }

      else if ((data.irc && data.irc.Received) ||
               (data.socket && data.socket.Error)) {

        function save (source) {
          newMessages[source] = (messages[source] || []).concat(message);
        }

        if (data.irc) {
          var msg = data.irc.Received;
          var command = msg.command;

          if (command == 'PRIVMSG') {
            save(message.getNameForArea());
            
          } else if (command == 'NICK') {
            for (var channelName in inhabitation)
              if (inhabitation[channelName].indexOf(msg.name) >= 0)
                save(channelName)
            
          } else if (Inhabitation.handle(inhabitation, message, save)) {
          
          } else if (command == 'PING') {
            return;

          } else {
            console.log(msg);
          }
        }

        if (data.socket && data.socket.Error)
          save(message.getNameForArea());

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
