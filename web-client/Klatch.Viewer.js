/** @jsx React.DOM */

(function () {
  var Replaying = Klatch.Replaying;
  var AreaSplitter = Klatch.AreaSplitter;

  function Inhabitation () {
    return {
      state: {},

      forUserChannels: function (msg, f) {
        var user = msg.irc.Received.prefix.User.nick;
        for (var key in this.state)
          if (this.state[key].indexOf(user) != -1)
            f(key);
      },

      update: function (_, msg) {
        var commands = {
          QUIT: function (name, msg) {
            for (var key in this.state)
              if (this.state[key].indexOf(name) != -1)
                this.state[key] = this.state[key].filter(
                  function (x) { return x != name; });
          },

          JOIN: function (name, msg) {
            var channel = msg.getChannelId();
            if (!this.state.hasOwnProperty(channel))
              this.state[channel] = [];

            this.state[channel].push(name);
          },

          PART: function (name, msg) {
            var channel = msg.getChannelId();

            if (!this.state.hasOwnProperty(channel))
              return;

            this.state[channel] = this.state[channel].filter(
              function (x) { return x != name; });
          }
        };

        var irc;
        if (irc = msg.irc ? msg.irc.Received : false) {
          if (commands.hasOwnProperty(irc.command)) {
            var nick = irc.prefix.User ? irc.prefix.User.nick : "[unknown]";
            commands[irc.command].call(this, nick, msg);
          }
        }
      }
    };
  };

  function ReplayStatus () {
    return {
      replaying: 0,
      replayed: 0,

      update: function (_, msg) {
        if (msg.meta && msg.meta.Replaying) {
          this.replaying = msg.meta.Replaying.count;
          this.replayed = 0;
        }

        else if (msg.meta && msg.meta.Streaming)
          this.replaying = this.replayed = 0;

        else if (this.replaying && !msg.meta)
          this.replayed += 1;
      }
    };
  }

  function Messages () {
    return {
      state: {},
      update: function (projections, msg) {
        if (msg.socket && msg.socket.Error)
          this.save(msg.getNameForArea(), msg);

        if (!msg.irc || !msg.irc.Received) return;

        var command = msg.irc.Received.command;
        var inhabitation = projections.inhabitation.state;

        if (command == 'PRIVMSG')
          this.save(msg.getNameForArea(), msg);
            
        else if (command == 'NICK') {
          for (var channelName in inhabitation)
            if (inhabitation[channelName].indexOf(msg.name) >= 0)
              this.save(channelName, msg)

        } else if (command == 'JOIN' || command == 'PART')
          this.save(msg.getChannelId(), msg);

        else if (command == 'QUIT')
          projections.inhabitation.forUserChannels(msg, function (area) {
            this.save(area, msg);
          }.bind(this));
      },

      save: function (source, msg) {
        if (!this.state.hasOwnProperty(source))
          this.state[source] = [];
        this.state[source].push(msg);
      }
    };
  }

  function Online () {
    return {
      state: false,
      update: function (_, msg) {
        if (msg.meta && msg.meta.hasOwnProperty('Online'))
          this.state = msg.meta.Online;
      }
    };
  }

  function Topics () {
    return {
      state: {},
      update: function (_, msg) {
        var irc;
        if (irc = (msg.irc && msg.irc.Received))
          if (irc.command == '332')
            this.state[msg.getChannelId(1)] = irc.trail;
      }
    };
  }

  function AreaMinimization () {
    return {
      state: {},
      update: function (_, msg) {
        var data;
        if (data = msg['klatch.js'])
          if (data.ToggleAreaMinimization) {
            source = data.ToggleAreaMinimization.area;
            this.state[source] = !this.state[source];
          }
      }
    };
  }

  Klatch.Viewer = React.createClass({
    getInitialState: function () {
      return {
        replayStatus: ReplayStatus(),
        messages: Messages(),
        areaMinimization: AreaMinimization(),
        inhabitation: Inhabitation(),
        topics: Topics(),
        online: Online()
      };
    },

    render: function () {
      if (this.state.replayStatus.replaying > 0) {
        var progress = 
          this.state.replayStatus.replayed / 
          this.state.replayStatus.replaying;

        return <Replaying progress={progress} />;
      } else
        return (
          <AreaSplitter messages={this.state.messages.state}
                        topics={this.state.topics.state}
                        areaMinimization={this.state.areaMinimization.state}
                        online={this.state.online.state} />
        );
    },

    recordMessage: function (data) {
      var message = Klatch.MessageModel(data);
      var state = this.state;

      state.areaMinimization.update(state, message);
      state.messages.update(state, message);
      state.inhabitation.update(state, message);
      state.replayStatus.update(state, message);
      state.online.update(state, message);
      state.topics.update(state, message);

      this.setState(state);
    }
  });
})();
