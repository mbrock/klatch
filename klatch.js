/** @jsx React.DOM */

(function () {
  var Viewer = React.createClass({
    getInitialState: function () {
      return { replaying: 0, messages: { } };
    },

    render: function () {
      if (this.state.replaying > 0)
        return <Replaying count={this.state.replaying} />;
      else
        return <MessageLog messages={this.state.messages} />;
    },

    updateReplayCount: function () {
      if (this.state.replaying)
        return this.state.replaying - 1;
      else
        return this.state.replaying;
    },

    recordMessage: function (message) {
      if (message.payload.tag === 'Received') {
        var source;
        var name = message.payload.contents[0];
        var cmd  = message.payload.contents[1];

        if (cmd.msgPrefix && cmd.msgPrefix.Right) {
          source = cmd.msgPrefix.Right;
        } else if (cmd.msgCmd === 'PRIVMSG') {
          source = cmd.msgParams[0];
        } else {
          source = 'Messages';
        }

        if (this.state.messages[source]) {
          var messages = Object.create(this.state.messages);
          messages[source] = messages[source].concat(message);
          this.setState({
            messages: messages,
            replaying: this.updateReplayCount()
          });
        }
      }
    }
  });

  var SourceSplitter = React.createClass({
    render: function () {
      var areas = [];
      var source;

      for (source in this.props.messages) {
        areas.push(<Area name={source}
                         messages={this.props.messages[source]});
      }

      return <div>{messages}</div>
    }
  };

  var Area = React.createClass({
    render: function () {
      var messages = this.props.messages.map(function (message) {
        if (message.payload.tag === "Received")
          return <IRCMessage message={message} key={message.sequence} />;
        else
          return <Message message={message} key={message.sequence} />;
      });

      return (<article>
               <h1>{this.props.name}</h1>
               <section>{messages}</section>
              </article>);
    }
  });

  var Replaying = React.createClass({
    render: function () {
      return (
        <span className="replaying"
          Replaying {this.props.count} events...
        </span>
      );
    }
  });

  var Timestamp = React.createClass({
    render: function () {
      var time = moment(this.props.t).format("YYYY MMM DD, HH:mm");
      return <span className="timestamp">{time}</span>;
    }
  });

  var ServerKey = React.createClass({
    render: function () {
      return <span className="server-key">{this.props.name}</span>;
    }
  });

  var Command = React.createClass({
    render: function () {
      return <span className="command">{this.props.command}</span>;
    }
  });

  var Trail = React.createClass({
    render: function () {
      return <span className="trail">{this.props.text}</span>;
    }
  });

  var Message = React.createClass({
    render: function () {
      return <div>
        <Timestamp t={this.props.message.timestamp} />
        {JSON.stringify(this.props.message.payload)}
      </div>;
    }
  });

  var IRCMessage = React.createClass({
    render: function () {
      var payload = this.props.message.payload;
      var name = payload.contents[0];
      var msg = payload.contents[1];

      return <div>
        <Timestamp t={this.props.message.timestamp} />
        <ServerKey name={name} />
        <Command command={msg.msgCmd} />
        <Trail text={msg.msgTrail} />
        </div>;
    }
  });

  var viewer = <Viewer />;

  var source = new EventSource("/");
  source.onmessage = function (e) {
    var data = JSON.parse(e.data);
    viewer.recordMessage(data);
  };

  React.renderComponent(viewer, log);
})();
