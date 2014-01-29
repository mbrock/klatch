(function () {

  function MessageModel (message) {
    message.__proto__ = MessageModel.prototype;
    return message;
  }

  Klatch.MessageModel = MessageModel;

  MessageModel.prototype.getNameForArea = function () {
    var msg = this.irc && this.irc.Received;
    if (msg)
      if (msg.prefix && msg.prefix.Server) {
        return msg.prefix.Server.host;
      } else if (msg.command === 'PRIVMSG') {
        return this.getChannelId();
      } else {
        return msg.name;
      }

    else if (this.socket && this.socket.Error)
      return this.socket.Error.name;

    else
      return '*Unknown*';
  };

  MessageModel.prototype.getChannelId = function (i) {
    var msg = this.irc && this.irc.Received;
    return msg.params[i || 0];
  },

  MessageModel.prototype.getServerName = function () {
    if (this.irc && this.irc.Received)
      return this.irc.Received.name;
  };

  MessageModel.prototype.getUserNick = function () {
    var msg = this.irc && this.irc.Received;
    return msg && msg.prefix && msg.prefix.User && msg.prefix.User.nick;
  };

  MessageModel.prototype.isBoring = function () {
    var msg = this.irc && this.irc.Received;
    return !(msg && msg.command === 'PRIVMSG');
  };
})();
