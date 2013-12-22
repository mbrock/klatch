(function () {

  function MessageModel (message) {
    this.message   = message;
    this.timestamp = message.timestamp;
    this.sequence  = message.sequence;
    this.payload   = message.payload;
    this.tag       = message.payload.tag;
    this.contents  = message.payload.contents;

    this.isMeta    = this.sequence === -1;
    this.isClient  = this.tag         === 'ClientEvent'
                  && this.contents[0] === 'klatch.js';

    if (this.isClient) {
      this.data = JSON.parse(this.contents[1]);
      this.clientEventTag = this.data.tag;
    }
  }

  Klatch.MessageModel = MessageModel;

  MessageModel.prototype.getServerName = function () {
    return this.contents[0];
  };

  MessageModel.prototype.getIRCMessage = function () {
    return this.contents[1];
  };

  MessageModel.prototype.getNameForArea = function () {
    var msg = this.contents[1];

    if (msg.msgPrefix && msg.msgPrefix.Right) {
      return msg.msgPrefix.Right;
    } else if (msg.msgCmd === 'PRIVMSG') {
      return msg.msgParams[0] + ' (' + this.contents[0] + ')';
    } else {
      return '*Server*';
    }
  };

  MessageModel.prototype.getUserNick = function () {
    if (this.contents[1] &&
        this.contents[1].msgPrefix &&
        this.contents[1].msgPrefix.Left)
      return this.contents[1].msgPrefix.Left.userNick
  };
})();
