/** @jsx React.DOM */
/*   -*- js2 -*-   */

(function () {
  var MessageLog = React.createClass({
    getInitialState: function () {
      return { messages: [] };
    },

    render: function () {
      var messages = this.state.messages.map(function (message) {
        if (message.payload.tag === "Received")
          return <IRCMessage message={message} key={message.sequence} />;
        else
          return <Message message={message} key={message.sequence} />;
      });

      return (<div> {messages} </div>);
    },

    recordMessage: function (message) {
      var messages = this.state.messages.concat([message]);
      this.setState({ messages: messages });
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

  var messageLog = <MessageLog />;

  var source = new EventSource("/");
  source.onmessage = function (e) {
    var data = JSON.parse(e.data);
    messageLog.recordMessage(data);
  };

  React.renderComponent(messageLog, log);
})();
