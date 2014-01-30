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
    var irc = this.irc && this.irc.Received;
    if (irc)
      return irc.params[i || 0];
    else if (this.socket && this.socket.Error)
      return this.socket.Error.name;
  },

  MessageModel.prototype.getAreaDescriptor = function (i) {
    return {
      name: this.getChannelId(i),
      server: this.getServerName()
    };
  },

  MessageModel.prototype.getAreaId = function (i) {
    return Klatch.areaId(this.getAreaDescriptor(i));
  },

  MessageModel.prototype.getServerName = function () {
    if (this.irc && this.irc.Received)
      return this.irc.Received.name;
    else if (this.socket && this.socket.Error)
      return this.socket.Error.name;
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
