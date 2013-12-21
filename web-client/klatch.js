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
    },

    assignColor: function (s) {
      var scalar = this.scalarHash(s);
      var colors = ["#b58900", "#cb4b16", "#dc322f", "#d33682",
                    "#6c71c4", "#268bd2", "#2aa198", "#859900"];
      return colors[Math.floor(scalar * colors.length)];
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
      var message = new MessageModel(data);
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
        messages[source] = (this.state.messages[source] || []).concat(message);

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

  var Replaying = React.createClass({
    render: function () {
      var curve = Math.pow(this.props.progress, 1.7);
      var ratio = curve * 100;
      var style = {
        width: (100 - ratio) + "%",
        opacity: (1 - curve)
      };
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
                         messages={this.props.messages[source]}
                         minimized={this.props.areaMinimization[source]} />);
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
                          minimized={this.props.minimized}
                          messages={messages} />
               : <Boring name={this.props.name}
                         minimized={this.props.minimized}
                         messages={messages} />;
    }
  });

  var AreaHeader = React.createClass({
    render: function () {
      var style = { "background-color": this.calculateColor() };

      return <h1 style={style}>
        <a href="#" rel="toggle-area-minimization" title="Hide/show"></a>
        <a href="#" rel="mark-as-read" title="Mark as read">✔︎</a>
        {this.props.name}
      </h1>;
    },

    calculateColor: function () {
      return CleverStuff.assignColor(this.props.name);
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

      $("a[rel=toggle-area-minimization]", node).click(function () {
        Klatch.saveClientEvent({
          tag: 'toggle-area-minimization',
          area: self.props.name
        });
      });
    }
  });

  var Channel = React.createClass({
    render: function () {
      var visibility = this.props.minimized ? ' minimized' : '';

      return (<article className={"area channel" + visibility}>
               <AreaHeader name={this.props.name} />
               <div>
                <section className="area-content">
                 {this.props.messages}
                </section>
                <input className="input" type="text" />
               </div>
              </article>);
    },

    componentDidMount: function (node) {
      $("input", node).focus();

      var lastReadMarker = $("hr", node).last();
      if (lastReadMarker[0]) {
        // how to make this work properly?
        $("section", node).scrollTop(100000);
      }
    }
  });

  var Boring = React.createClass({
    render: function () {
      var visibility = this.props.minimized ? ' minimized' : '';

      return (<article className={"area boring" + visibility}>
               <AreaHeader name={this.props.name}
                           minimized={this.props.minimized} />
               <div>
                <section className="area-content">
                 {this.props.messages}
                </section>
               </div>
              </article>);
    },
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
      var source, citeStyle;

      if (this.props.by) {
        citeStyle = { color: CleverStuff.assignColor(this.props.by) };
        source = <cite style={citeStyle}>{this.props.by}</cite>;
      }
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
