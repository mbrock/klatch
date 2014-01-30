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

      var isChannel = this.props.area.name.match(/^#/);

      return isChannel ?
                 <Channel area={this.props.area}
                          topic={this.props.topic}
                          minimized={this.props.minimized ? true : false}
                          messages={messages} />
               : <Boring area={this.props.area}
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

        if (Klatch.projectionState("HideBoringStuff")[source])
          messages = messages.filter(function (x) { return !x.isBoring() });

        messages = messages.slice(messages.length - 1000);
        areas.push(<Area area={Klatch.fromAreaId(source)}
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
      var name = this.props.area.name;

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
      return Klatch.Clever.assignColor(Klatch.areaId(this.props.area));
    },

    componentDidMount: function (node) {
      var self = this;
      $("a[rel=mark-as-read]", node).click(function () {
        Klatch.recordClientEvent({
          MarkAsRead: { area: self.props.area }
        });
      });

      $("a[rel=toggle-area-minimization]", node).click(function () {
        Klatch.recordClientEvent({
          ToggleAreaMinimization: self.props.area
        });
      });
    }
  });

  var Channel = Klatch.Channel = React.createClass({
    render: function () {
      var visibility = this.props.minimized ? ' minimized' : '';

      var messages;

      var inhabitation = Klatch.projectionState("Inhabitation");
      var usersHere = inhabitation[Klatch.areaId(this.props.area)];

      if (!this.props.minimized)
        messages = <div>
                    <section className="area-content" ref="content">
                     {this.props.messages}
                    </section>
                    <InputBar area={this.props.area} users={usersHere} />
                   </div>;

      return (<article className={"area channel" + visibility}>
               <AreaHeader area={this.props.area}
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
      if (Klatch.projectionState("ScrollLock")
          [Klatch.areaId(this.props.area)]) return;

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
               <AreaHeader area={this.props.area}
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
      var match;

      if (text === "/hide-boring-stuff")
        Klatch.recordClientEvent({ HideBoringStuff: this.props.area });

      else if (text === "/scroll-lock")
        Klatch.recordClientEvent({ ScrollLock: this.props.area });

      else if (match = text.match(/^\/set-theme (.*)$/))
        Klatch.recordClientEvent({ SetTheme: match[1] });

      else {
        var msg = "PRIVMSG " + this.props.area.name + " :" + text;
        Klatch.sendCommand({
          line: { Send: { name: this.props.area.server, line: msg }}
        });

        Klatch.recordClientEvent({
          Received: {
            name: this.props.area.server,
            prefix: { User: { nick: "me" } },
            command: "PRIVMSG",
            params: [this.props.area.name],
            trail: text
          }
        }, "irc");
      }
    }
  });
})();
