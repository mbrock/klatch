/** @jsx React.DOM */
/*   -*- js2 -*-   */

(function () {
  var MessageLog = React.createClass({
    getInitialState: function () {
      return { messages: [] };
    },

    render: function () {
      var messages = this.state.messages.map(function (message) {
        return <Message message={message} key={message.sequence} />;
      });

      return (<ol> {messages} </ol>);
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

  var Message = React.createClass({
    render: function () {
      return <li>
        <Timestamp t={this.props.message.timestamp} />
        {JSON.stringify(this.props.message.payload)}
      </li>;
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
