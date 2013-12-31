/** @jsx React.DOM */

(function () {
  Klatch.IRCMessage = React.createClass({
    render: function () {
      var message = this.props.message;
      var name = message.getServerName();
      var irc = message.irc.Received;

      var source;
      var content;
      var timestamp;

      if (message.timestamp % 4 == 0) {
        timestamp = <Timestamp t={message.timestamp} />;
      }

      if (irc.command === 'PRIVMSG' && (source = message.getUserNick())) {
        source = this.props.sourceDiffers ? source: null;
        content = <Utterance by={source} text={irc.trail} />;

      } else {
        content = <span>
          <ServerKey name={name} />
          <Command command={irc.command} />
          <Trail text={irc.trail} />;
        </span>;
      }

      return <div> {timestamp} {content} </div>;
    }
  });

  Klatch.ErrorMessage = React.createClass({
    render: function () {
      return (
        <div className="error-message">
          <Timestamp t={this.props.message.timestamp} />
          <span>
            <ServerKey name={this.props.message.socket.Error.name} />
            <Command command="(error)" />
            <Trail text={this.props.message.socket.Error.reason} />
          </span>
        </div>
      );
    }
  });

  Klatch.Message = React.createClass({
    render: function () {
      return <div>
        {JSON.stringify(this.props.message)}
      </div>;
    }
  });

  Klatch.MarkedAsRead = React.createClass({
    render: function () { return <hr />; }
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

  var Utterance = React.createClass({
    render: function () {
      var source, citeStyle;

      if (this.props.by) {
        citeStyle = { color: Klatch.Clever.assignColor(this.props.by) };
        source = <cite style={citeStyle}>{this.props.by}</cite>;
      }
      return <p>{source}<span>{this.props.text}</span></p>;
    }
  });
})();
