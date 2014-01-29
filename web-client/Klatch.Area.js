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

      var isChannel = this.props.name.match(/^#/);
      var descriptor = isChannel
        ? this.props.name
        : null;

      return isChannel ?
                 <Channel area={descriptor}
                          topic={this.props.topic}
                          minimized={this.props.minimized ? true : false}
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
        messages = messages.slice(messages.length - 1000);
        areas.push(<Area name={source}
                         topic={this.props.topics[source]}
                         messages={messages}
                         minimized={this.props.areaMinimization[source]} />);
      }

      return <div className="area-splitter">
        <header>
          {this.props.online.online ? "Online" : "Offline"}
          ({this.props.online.messagesHandled} messages)
        </header>
        {areas}
      </div>;
    }
  });

  var AreaHeader = Klatch.AreaHeader = React.createClass({
    render: function () {
      var style = { "background-color": this.calculateColor() };
      var name = this.props.name;

      if (name.match(/^\w/))
        name = " " + name;

      var topic = this.props.topic && <h2>{this.props.topic}</h2>;

      return <div>
        <h1 style={style}>
          <a href="#" rel="toggle-area-minimization" title="Hide/show"></a>
          <a href="#" rel="mark-as-read" title="Mark as read">✔︎</a>
          <span>{name}</span>
        </h1>
        {topic}
      </div>;
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
            area: self.props.name,
            server: self.props.server
          }
        });
      });
    }
  });

  var Channel = Klatch.Channel = React.createClass({
    render: function () {
      var visibility = this.props.minimized ? ' minimized' : '';

      var messages;

      var inhabitation = Klatch.projectionState("Inhabitation");
      var usersHere = inhabitation[this.props.area];

      if (!this.props.minimized)
        messages = <div>
                    <section className="area-content" ref="content">
                     {this.props.messages}
                    </section>
                    <InputBar area={this.props.area} users={usersHere} />
                   </div>;

      return (<article className={"area channel" + visibility}>
               <AreaHeader name={this.props.area}
                           topic={this.props.topic} />
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
      if (this.props.minimized) return;
      var content = this.refs.content.getDOMNode();
      $(content).animate({
        scrollTop: content.scrollHeight
      }, 600);
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
      // horrible

      var msg = "PRIVMSG " + this.props.area + " :" + text;
      Klatch.sendCommand({
        line: { Send: { name: "freenode", line: msg }}
      });

      Klatch.recordClientEvent({
        Received: {
          name: "freenode",
          prefix: { User: { nick: "klatch" } },
          command: "PRIVMSG",
          params: [this.props.area],
          trail: text
        }
      }, "irc");
    }
  });
})();
