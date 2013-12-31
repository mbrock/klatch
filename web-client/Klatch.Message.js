(function () {

  function MessageModel (message) {
    message.__proto__ = MessageModel.prototype;
    return message;
  }

  Klatch.MessageModel = MessageModel;

  MessageModel.prototype.getNameForArea = function () {
    var msg = this.irc;
    if (this.irc)
      if (this.irc.prefix && this.irc.prefix.Server) {
        return this.irc.prefix.Server.host;
      } else if (this.irc.command === 'PRIVMSG') {
        return this.irc.params[0] + ' (' + this.irc.name + ')';
      } else {
        return '*Server*';
      }

    else if (this.socket && this.socket.Error)
      return this.socket.Error.name;

    else
      return '*Unknown*';
  };

  MessageModel.prototype.getServerName = function () {
    if (this.irc && this.irc.Received)
      return this.irc.Received.name;
  };

  MessageModel.prototype.getUserNick = function () {
    var msg = this.irc;
    return msg && msg.prefix && msg.prefix.User && msg.prefix.User.nick;
  };
})();
