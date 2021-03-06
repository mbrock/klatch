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
        source = this.props.sourceDiffers ? source : null;
        content = <Utterance by={source} text={irc.trail} />;

      } else if (irc.prefix.User && (source = message.getUserNick())) {
        source = this.props.sourceDiffers ? source : null;
        content = <UserAction by={source} msg={irc}/>;

      } else {
        content = <span>
          <Command command={irc.command} />
          <Params params={irc.params} />
          <Trail text={irc.trail} />
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
  var Params    = React.createClass({
    render: function () {
      return <span className="params">{this.props.params.join(" ")}</span>;
    }
  });

  var Utterance = React.createClass({
    render: function () {
      var source, citeStyle, text;

      if (this.props.by) {
        citeStyle = { color: Klatch.Clever.assignColor(this.props.by) };
        source = <cite style={citeStyle}>{this.props.by}</cite>;
      }

      text = Klatch.Clever.prettifyHaskell(this.props.text);
      
      return <p>{source}<span>{text}</span></p>;
    }
  });

  var UserAction = React.createClass({
    render: function () {
      var source, citeStyle, text;

      if (this.props.by) {
        citeStyle = { color: Klatch.Clever.assignColor(this.props.by) };
        source = <span style={citeStyle}>{this.props.by}</span>;
      }

      text = <span>{this.props.msg.command}</span>;

      return <p>{source} {text}</p>;
    }
  });
 
})();
