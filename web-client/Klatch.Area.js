/** @jsx React.DOM */

(function () {
  var IRCMessage   = Klatch.IRCMessage;
  var Message      = Klatch.Message;
  var ErrorMessage = Klatch.ErrorMessage;
  var MarkedAsRead = Klatch.MarkedAsRead;

  var Area = Klatch.Area = React.createClass({
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

        if (message.irc && message.irc.Received)
          return <IRCMessage message={message}
                             sourceDiffers={sourceDiffers || !(i++ % 5)}
                             key={message.sequence} />;

        else if (message.socket && message.socket.Error)
          return <ErrorMessage message={message} key={message.sequence} />;

        else if (message['klatch.js'] && message['klatch.js'].MarkAsRead)
          return <MarkedAsRead key={message.sequence} />;

        else
          return <Message message={message} key={message.sequence} />;
      });

      var isChannel = this.props.name.match(/^(#.*) \((.*)\)$/);
      var descriptor = isChannel
        ? { name: isChannel[1], server: isChannel[2] }
        : null;

      return isChannel ?
                 <Channel area={descriptor}
                          minimized={this.props.minimized}
                          messages={messages} />
               : <Boring name={this.props.name}
                         minimized={this.props.minimized}
                         messages={messages} />;
    }
  });

  Klatch.AreaSplitter = React.createClass({
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

  var AreaHeader = Klatch.AreaHeader = React.createClass({
    render: function () {
      var style = { "background-color": this.calculateColor() };
      var name = this.props.name;

      if (name.match(/^\w/))
        name = " " + name;

      return <h1 style={style}>
        <a href="#" rel="toggle-area-minimization" title="Hide/show"></a>
        <a href="#" rel="mark-as-read" title="Mark as read">✔︎</a>
        <span>{name}</span>
      </h1>;
    },

    calculateColor: function () {
      return Klatch.Clever.assignColor(this.props.name);
    },

    componentDidMount: function (node) {
      var self = this;
      $("a[rel=mark-as-read]", node).click(function () {
        Klatch.recordClientEvent({
          MarkAsRead: {
            area: self.props.name,
            server: self.props.server
          }
        });
      });

      $("a[rel=toggle-area-minimization]", node).click(function () {
        Klatch.recordClientEvent({
          ToggleAreaMinimization: {
            area: self.props.name
          }
        });
      });
    }
  });

  var Channel = Klatch.Channel = React.createClass({
    render: function () {
      var visibility = this.props.minimized ? ' minimized' : '';

      var messages;

      if (!this.props.minimized)
        messages = <div>
                    <section className="area-content" ref="content">
                     {this.props.messages}
                    </section>
                    <InputBar area={this.props.area} />
                   </div>;

      return (<article className={"area channel" + visibility}>
               <AreaHeader name={this.props.area.name} />
               {messages}
              </article>);
    },

    componentDidUpdate: function () {
      this.scrollDown();
    },

    componentDidMount: function () {
      this.scrollDown();
    },

    scrollDown: function () {
      var content = this.refs.content.getDOMNode();
      $(content).animate({
        scrollTop: content.scrollHeight
      }, 250);
    }
  });

  var Boring = Klatch.Boring = React.createClass({
    render: function () {
      var visibility = this.props.minimized ? ' minimized' : '';
      var messages;

      if (!this.props.minimized)
        messages = <div>
                    <section className="area-content">
                     {this.props.messages}
                    </section>
                   </div>;

      return (<article className={"area boring" + visibility}>
               <AreaHeader name={this.props.name}
                           minimized={this.props.minimized} />
               {messages}
              </article>);
    },
  });

  var InputBar = Klatch.InputBar = React.createClass({
    render: function () {
      return <input className="input" type="text" tabIndex="0" />;
    },

    componentDidMount: function (node) {
      $(node).keydown(this.handleKey.bind(this, $(node)));
    },

    handleKey: function (node, e) {
      if (e.keyCode === 13) {
        this.handleCommand(node.val());
        node.val("");
      }
    },

    handleCommand: function (text) {
      var msg = "PRIVMSG " + this.props.area.name + " :" + text;
      Klatch.sendCommand({
        line: { Send: { name: this.props.area.server, line: msg }}
      });
    }
  });
})();
