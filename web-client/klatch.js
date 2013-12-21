/** @jsx React.DOM */

(function () {
  var Klatch = {
    ClientTag: 'klatch.js',
    ClientVersion: '0.0.1',

    saveClientEvent: function (event) {
      event.version = Klatch.ClientVersion;
      return $.post('/', JSON.stringify({
        tag: 'SaveClientEvent',
        contents: [Klatch.ClientTag, JSON.stringify(event)]
      }));
    }
  };

  var CleverStuff = {
    scalarHash: function (s) {
      // NOTE: Highly bogus, not actually clever.
      var h = i = c = 0;
      while (i++ < s.length) {
        c = s.charCodeAt(i);
        h ^= (c % 7) << 5;
        h ^= (c % 37) << 4;
        h ^= (c % 97) << 3;
        h ^= (c % 137) << 2;
        h ^= (c % 251) << 1;
        h = h % 37;
      }
      return h / 37.0;
    }
  };

  function MessageModel (message) {
    this.message   = message;
    this.timestamp = message.timestamp;
    this.sequence  = message.sequence;
    this.payload   = message.payload;
    this.tag       = message.payload.tag;
    this.contents  = message.payload.contents;

    this.isMeta    = this.sequence === -1;
    this.isClient  = this.tag         === 'ClientEvent'
                  && this.contents[0] === 'klatch.js';

    if (this.isClient) {
      this.data = JSON.parse(this.contents[1]);
      this.clientEventTag = this.data.tag;
    }
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

      else if (message.payload.tag === 'ClientEvent') {
        messages = this.handleClientEvent(message.data.tag, message);
        replaying = this.state.replaying;
        replayed = this.updateReplayCount(false);
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
    },

    handleClientEvent: function (tag, message) {
      var messages;
      var source;

      if (tag === 'mark-as-read') {
        source = message.data.area;
        messages = Object.create(this.state.messages);
        messages[source] = (messages[source] || []).concat(message);
      } else {
        console.log("Unrecognized client event '%o': %o", tag, message);
      }

      return messages;
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
        else if (message.clientEventTag === 'mark-as-read')
          return <MarkedAsRead />;
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

  var AreaHeader = React.createClass({
    render: function () {
      var style = {
        "background-color": this.calculateColor()
      };

      return <h1 style={style}>
        {this.props.name}
        (<a href="#" rel="mark-as-read">Mark as read</a>)
      </h1>;
    },

    calculateColor: function () {
      var scalar = CleverStuff.scalarHash(this.props.name);
      var colors = ["#b58900", "#cb4b16", "#dc322f", "#d33682",
                    "#6c71c4", "#268bd2", "#2aa198", "#859900"];
      return colors[Math.round(scalar * colors.length)];
    },

    componentDidMount: function (node) {
      var self = this;
      $("a[rel=mark-as-read]", node).click(function () {
        Klatch.saveClientEvent({
          tag: 'mark-as-read',
          area: self.props.name,
          server: self.props.server
        });
      });
    }
  });

  var Channel = React.createClass({
    render: function () {
      return (<article className="channel">
               <AreaHeader name={this.props.name} />
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
               <AreaHeader name={this.props.name} />
               <section>{this.props.messages}</section>
              </article>);
    },

    componentDidMount: function (node) {
      $(node).scrollTop($("section", node).height() + 10);
    }
  });

  var MarkedAsRead = React.createClass({
    render: function () { return <hr />; }
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
