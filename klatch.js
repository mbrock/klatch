/** @jsx React.DOM */

(function () {
  var Viewer = React.createClass({
    getInitialState: function () {
      return { replaying: 0, replayed: 0, messages: { } };
    },

    render: function () {
      if (this.state.replaying > 0) {
        var progress = this.state.replayed / this.state.replaying;
        return <Replaying progress={progress} />;
      } else
        return <AreaSplitter messages={this.state.messages} />;
    },

    updateReplayCount: function (isMetamessage) {
      if (this.state.replaying && !isMetamessage)
        return this.state.replayed + 1;
      else
        return this.state.replayed;
    },

    recordMessage: function (message) {
      var messages;
      var replaying, replayed;
      var isMetamessage = message.sequence === -1;
      var source;
      var name;
      var cmd;

      if (message.payload.tag === 'Replaying') {
        messages = this.state.messages;
        replaying = message.payload.contents;
        replayed = 0;
      }

      else if (message.payload.tag === 'Received') {
        name = message.payload.contents[0];
        cmd  = message.payload.contents[1];

        if (cmd.msgPrefix && cmd.msgPrefix.Right) {
          source = cmd.msgPrefix.Right;
        } else if (cmd.msgCmd === 'PRIVMSG') {
          source = cmd.msgParams[0];
        } else {
          source = 'Messages';
        }

        messages = Object.create(this.state.messages);
        messages[source] = (messages[source] || []).concat(message);
        replaying = this.state.replaying;
        replayed = this.updateReplayCount(isMetamessage);
      }

      else {
        messages = this.state.messages;
        replaying = this.state.replaying;
        replayed = this.updateReplayCount(isMetamessage);
      }

      if (replayed === replaying)
        replayed = replaying = 0;

      this.setState({
        messages: messages,
        replaying: replaying,
        replayed: replayed
      });
    }
  });

  var AreaSplitter = React.createClass({
    render: function () {
      var areas = [];
      var source, messages;

      for (source in this.props.messages) {
        messages = this.props.messages[source];
        areas.push(<Area name={source}
                         messages={this.props.messages[source]} />);
      }

      return <div className="area-splitter">{areas}</div>;
    }
  });

  var Area = React.createClass({
    render: function () {
      var source;
      var newSource;
      var sourceDiffers = true;
      var i = 0;

      var messages = this.props.messages.map(function (message) {

        if (message.payload.contents[1] &&
            message.payload.contents[1].msgPrefix &&
            message.payload.contents[1].msgPrefix.Left &&
            message.payload.contents[1].msgPrefix.Left.userNick) {

          newSource     = message.payload.contents[1].msgPrefix.Left.userNick;
          sourceDiffers = source !== newSource;
          source        = newSource;
        }

        if (message.payload.tag === "Received")
          return <IRCMessage message={message}
                             sourceDiffers={sourceDiffers || !(i++ % 5)}
                             key={message.sequence} />;
        else
          return <Message message={message} key={message.sequence} />;
      });

      var isChannel = this.props.name.match(/^#/);
      return isChannel ?
                 <Channel name={this.props.name}
                          messages={messages} />
               : <Boring name={this.props.name}
                         messages={messages} />;
    }
  });

  var Channel = React.createClass({
    render: function () {
      return (<article className="channel">
               <h1>{this.props.name}</h1>
               <section>{this.props.messages}</section>
               <input className="input" type="text" />
              </article>);
    },

    componentDidMount: function (node) {
      $(node).scrollTop($("section", node).height() + 10);
      $("input", node).focus();
    }
  });

  var Boring = React.createClass({
    render: function () {
      return (<article className="boring">
               <h1>{this.props.name}</h1>
               <section>{this.props.messages}</section>
              </article>);
    },

    componentDidMount: function (node) {
      $(node).scrollTop($("section", node).height() + 10);
    }
  });

  var Replaying = React.createClass({
    render: function () {
      var curve = Math.pow(this.props.progress, 1.7);
      var ratio = curve * 100;
      var style = { width: (100 - ratio) + "%"};
      return (
        <div className="replaying">
          <span className="replaying" style={style}></span>
        </div>
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
      var content;
      var timestamp;

      if (this.props.message.timestamp % 1000000 == 0) {
        timestamp = <Timestamp t={this.props.message.timestamp} />;
      }

      return <div>
        {JSON.stringify(this.props.message.payload)}
      </div>;
    }
  });

  var Utterance = React.createClass({
    render: function () {
      var source = this.props.by ? <cite>{this.props.by}</cite> : null;
      return <p>{source}<span>{this.props.text}</span></p>;
    }
  });

  var IRCMessage = React.createClass({
    render: function () {
      var payload = this.props.message.payload;
      var name = payload.contents[0];
      var msg = payload.contents[1];
      var source;

      var content;
      var timestamp;

      if (this.props.message.timestamp % 4 == 0) {
        timestamp = <Timestamp t={this.props.message.timestamp} />;
      }

      if (msg.msgCmd === 'PRIVMSG') {
        if (msg.msgPrefix && msg.msgPrefix.Left) {
          source = this.props.sourceDiffers ? msg.msgPrefix.Left.userNick : null;
        }
        content = <Utterance by={source} text={msg.msgTrail} />;

      } else {
        content = <span>
          <ServerKey name={name} />
          <Command command={msg.msgCmd} />
          <Trail text={msg.msgTrail} />;
        </span>;
      }

      return <div>
        {timestamp}
        {content}
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
