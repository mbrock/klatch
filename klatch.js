/** @jsx React.DOM */

(function () {
  function MessageModel (message) {
    this.message   = message;
    this.timestamp = message.timestamp;
    this.sequence  = message.sequence;
    this.payload   = message.payload;
    this.tag       = message.payload.tag;
    this.contents  = message.payload.contents;

    this.isMeta    = this.sequence === -1;
  }

  (function () {
    MessageModel.prototype.getServerName = function () {
      return this.contents[0];
    };

    MessageModel.prototype.getIRCMessage = function () {
      return this.contents[1];
    };

    MessageModel.prototype.getNameForArea = function () {
      var msg = this.contents[1];

      if (msg.msgPrefix && msg.msgPrefix.Right) {
        return msg.msgPrefix.Right;
      } else if (msg.msgCmd === 'PRIVMSG') {
        return msg.msgParams[0];
      } else {
        return 'Messages';
      }
    };

    MessageModel.prototype.getUserNick = function () {
      if (this.contents[1] &&
          this.contents[1].msgPrefix &&
          this.contents[1].msgPrefix.Left)
        return this.contents[1].msgPrefix.Left.userNick
    };
  })();

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

    recordMessage: function (data) {
      var message = new MessageModel(data);
      var messages;
      var replaying, replayed;
      var source;
      var name;
      var cmd;

      if (message.payload.tag === 'Replaying') {
        messages = this.state.messages;
        replaying = message.contents;
        replayed = 0;
      }

      else if (message.payload.tag === 'Received') {
        source = message.getNameForArea();

        messages = Object.create(this.state.messages);
        messages[source] = (messages[source] || []).concat(message);
        replaying = this.state.replaying;
        replayed = this.updateReplayCount(message.isMeta);
      }

      else {
        messages = this.state.messages;
        replaying = this.state.replaying;
        replayed = this.updateReplayCount(message.isMeta);
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
        newSource = message.getUserNick();

        if (newSource) {
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

  var IRCMessage = React.createClass({
    render: function () {
      var message = this.props.message;
      var name = message.getServerName();
      var msg = message.getIRCMessage();

      var source;
      var content;
      var timestamp;

      if (message.timestamp % 4 == 0) {
        timestamp = <Timestamp t={message.timestamp} />;
      }

      if (msg.msgCmd === 'PRIVMSG' && (source = message.getUserNick())) {
        source = this.props.sourceDiffers ? source: null;
        content = <Utterance by={source} text={msg.msgTrail} />;

      } else {
        content = <span>
          <ServerKey name={name} />
          <Command command={msg.msgCmd} />
          <Trail text={msg.msgTrail} />;
        </span>;
      }

      return <div> {timestamp} {content} </div>;
    }
  });

  var Utterance = React.createClass({
    render: function () {
      var source = this.props.by ? <cite>{this.props.by}</cite> : null;
      return <p>{source}<span>{this.props.text}</span></p>;
    }
  });

  var Timestamp = React.createClass({
    render: function () {
      var time = moment(this.props.t).format("YYYY MMM DD, HH:mm");
      return <span className="timestamp">{time}</span>;
    }
  });

  function createSpanClass(className, contentKey) {
    return React.createClass({
      render: function () {
        return <span className={className}>{this.props[contentKey]}</span>;
      }
    });
  }

  var ServerKey = createSpanClass("server-key", "name");
  var Command   = createSpanClass("command", "command");
  var Trail     = createSpanClass("trail", "text");

  var Message = React.createClass({
    render: function () {
      return <div>
        {JSON.stringify(this.props.message.payload)}
      </div>;
    }
  });

  var viewer = <Viewer />;

  (function downloadEvents () {
    var source = new EventSource("/");
    source.onmessage = function (e) {
      var data = JSON.parse(e.data);
      viewer.recordMessage(data);
    };
  })();

  React.renderComponent(viewer, log);
})();
